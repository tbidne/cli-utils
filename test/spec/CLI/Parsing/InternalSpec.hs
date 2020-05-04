{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module CLI.Parsing.InternalSpec
  ( spec,
  )
where

import CLI.Parsing.Internal
import CLI.Types.Env
import Common.Parsing.ParseAnd
import qualified Common.RefinedUtils as R
import Common.Utils
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "CLI.Parsing.InternalSpec" $ do
    it "Empty args uses defaults" $ do
      pureParseArgs [] `shouldSatisfy` verifyDefaults
    prop "Correctly parses valid args" parsesArgs
    prop "Correctly parses legend map" vMapStrToEnv
    prop "Invalid map line dies" vInvalidMapLine
    prop "Invalid timeout dies" vInvalidTimeout

verifyDefaults :: ParseAnd Acc -> Bool
verifyDefaults =
  \case
    (ParseAnd (PFailure _)) -> False
    (ParseAnd (PSuccess (Acc {accLegend, accTimeout, accCommands}))) ->
      accLegend == Nothing
        && accTimeout == Nothing
        && accCommands == []

parsesArgs :: ValidArgs -> Bool
parsesArgs (ValidArgs legend timeout commands) =
  case pureParseArgs (legend : timeout : commands) of
    (ParseAnd (PFailure _)) -> False
    (ParseAnd (PSuccess (Acc {accLegend, accTimeout, accCommands}))) ->
      verifyLegendPath legend accLegend
        && verifyCommands commands accCommands
        && verifyTimeout timeout accTimeout

verifyLegendPath :: String -> Maybe FilePath -> Bool
verifyLegendPath "--legend=" Nothing = True
verifyLegendPath (matchAndStrip "--legend=" -> Just s) (Just s') = s == s'
verifyLegendPath _ _ = False

verifyCommands :: [String] -> [T.Text] -> Bool
verifyCommands xs ts = and (fmap f ts)
  where
    strSet = S.fromList $ fmap T.pack xs
    f = flip S.member strSet

verifyTimeout :: String -> Maybe (R.RNonNegative Int) -> Bool
verifyTimeout "" Nothing = True
verifyTimeout (matchAndStrip "--timeout=" -> Just s) (Just t) =
  (read s) == R.unrefine t
verifyTimeout _ _ = False

vMapStrToEnv :: [ValidMapLine] -> Bool
vMapStrToEnv validLines =
  let mapStr = intercalate (fmap unvalidMapLine validLines) "\n"
      -- this seems dumb but we need to do this after intercalate
      -- because the lines themselves could have '\n' in them
      numLines = length $ lines mapStr
   in case mapStrToEnv [] Nothing mapStr of
        Right (Env mp Nothing _) ->
          -- key size of the map <= number of text lines
          -- potential duplicate keys is why we can't check simple equality
          length mp <= numLines
        _ -> False

vInvalidMapLine :: InvalidMapLines -> Bool
vInvalidMapLine (InvalidMapLines (_, _, allLines)) =
  case mapStrToEnv [] Nothing (intercalate allLines "\n") of
    Left _ -> True
    _ -> False

vInvalidTimeout :: InvalidTimeout -> Bool
vInvalidTimeout (InvalidTimeout t) =
  case pureParseArgs [t] of
    (ParseAnd (PFailure _)) -> True
    _ -> False

data ValidArgs
  = ValidArgs
      { validLegendPath :: String,
        validTimeout :: String,
        validCommands :: [String]
      }
  deriving (Show)

instance Arbitrary ValidArgs where
  arbitrary = do
    p <- genLegendPath
    t <- genTimeout
    cs <- genCommands
    pure $ ValidArgs p t cs
    where
      genLegendPath = do
        (PrintableString s) <- arbitrary
        pure $ "--legend=" <> s
      genCommands = fmap getPrintableString <$> listOf arbitrary
      genTimeout = do
        (NonNegative n) <- arbitrary :: Gen (NonNegative Int)
        pure $ "--timeout=" <> show n
  shrink (ValidArgs "" "" []) = []
  shrink (ValidArgs _ "" []) = []
  shrink (ValidArgs "" _ []) = []
  shrink (ValidArgs "" "" [_]) = []
  shrink (ValidArgs "" "" (c : cs)) = [ValidArgs "" "" [c], ValidArgs "" "" cs]
  shrink (ValidArgs p t cs) =
    -- shrink to legend
    [ ValidArgs p "" [],
      -- shrink to timeout
      ValidArgs "" t [],
      -- shrink to commands
      ValidArgs "" "" cs
    ]

newtype ValidMapLine = ValidMapLine {unvalidMapLine :: String}
  deriving (Show)

instance Arbitrary ValidMapLine where
  arbitrary =
    ValidMapLine . getPrintableString
      <$> arbitrary `suchThat` (validMapLine . getPrintableString)

newtype InvalidMapLine = InvalidMapLine String deriving (Show)

instance Arbitrary InvalidMapLine where
  arbitrary =
    InvalidMapLine . getPrintableString
      <$> arbitrary `suchThat` (not . validMapLine . getPrintableString)

-- (badLine, goodLines, all in random order)
-- we carry around all info for shrinking
newtype InvalidMapLines = InvalidMapLines (String, [String], [String]) deriving (Show)

instance Arbitrary InvalidMapLines where
  arbitrary = do
    goodLines <- listOf $ fmap unvalidMapLine arbitrary
    (InvalidMapLine bad) <- arbitrary
    order <- shuffle (bad : goodLines)
    pure $ InvalidMapLines (bad, goodLines, order)

  -- on shrink, forget the random order and just shrink the good lines,
  -- adding the bad line to the beginning
  shrink (InvalidMapLines (_, [], _)) = []
  shrink (InvalidMapLines (bad, (g : gs), _)) =
    [InvalidMapLines (bad, gs, [bad, g]), InvalidMapLines (bad, gs, (bad : gs))]

intercalate :: Monoid m => [m] -> m -> m
intercalate [] y = y
intercalate (x : xs) y = x <> y <> intercalate xs y

-- A valid line can be one of
--   1. empty
--   2. single new line
--   3. start with '#'
--   4. s with exactly one equals (non-empty both sides)
-- If a non-empty line contains a new line then we recursively
-- check that each side is valid
validMapLine :: String -> Bool
validMapLine "" = True
validMapLine "\n" = True
validMapLine ('#' : _) = True
validMapLine ('=' : _) = False
validMapLine s = go s False
  where
    go [] b = b
    go "=" _ = False
    go ('\n' : xs) b = b && validMapLine xs
    go ('=' : _) True = False
    go ('=' : xs) False = go xs True
    go (_ : xs) b = go xs b

newtype InvalidTimeout = InvalidTimeout String deriving (Show)

instance Arbitrary InvalidTimeout where
  arbitrary = do
    (PrintableString s) <- arbitrary
    (Negative n) <- arbitrary :: Gen (Negative Int)
    -- empty, negative, and unparsable string should all die
    InvalidTimeout
      <$> elements
        [ "--timeout=",
          "--timeout=" <> show n,
          "--timeout=a" <> s
        ]
