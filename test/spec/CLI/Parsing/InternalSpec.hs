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
import Common.Utils
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "CLI.Parsing.Internal" $ do
    it "Empty args uses defaults" $ do
      pureParseArgs [] `shouldSatisfy` verifyDefaults
    prop "Correctly parses valid args" parsesArgs
    prop "Correctly parses legend map" vMapStrToEnv
    prop "Invalid map line dies" vInvalidMapLine

verifyDefaults :: ParseAnd Acc -> Bool
verifyDefaults =
  \case
    (ParseAnd (PFailure _)) -> False
    (ParseAnd (PSuccess (Acc {legendPath, cmds}))) ->
      legendPath == Nothing
        && cmds == []

parsesArgs :: ValidArgs -> Bool
parsesArgs ValidArgs {validLegendPath, validCommands, order} =
  case pureParseArgs order of
    (ParseAnd (PFailure _)) -> False
    (ParseAnd (PSuccess (Acc {legendPath, cmds}))) ->
      verifyLegendPath validLegendPath legendPath
        && verifyCommands validCommands cmds

verifyLegendPath :: String -> Maybe FilePath -> Bool
verifyLegendPath "--legend=" Nothing = True
verifyLegendPath (matchAndStrip "--legend=" -> Just s) (Just s') = s == s'
verifyLegendPath _ _ = False

verifyCommands :: [String] -> [T.Text] -> Bool
verifyCommands xs ts = and (fmap f ts)
  where
    strSet = S.fromList $ fmap T.pack xs
    f = flip S.member strSet

vMapStrToEnv :: [String] -> ValidMapLines -> Bool
vMapStrToEnv commands (ValidMapLines (numUnique, strs)) =
  let txtCommands = fmap T.pack commands
      mapTxt = intercalate strs "\n"
   in case mapStrToEnv txtCommands mapTxt of
        Left _ -> False
        Right (Env mp commands') ->
          -- all commands are added to Env
          txtCommands == commands'
            -- unique text keys = key size of the map
            && length mp == numUnique

vInvalidMapLine :: InvalidMapLines -> Bool
vInvalidMapLine (InvalidMapLines ls) =
  case mapStrToEnv [] (intercalate ls "\n") of
    Left _ -> True
    _ -> False

data ValidArgs
  = ValidArgs
      { validLegendPath :: String,
        validCommands :: [String],
        order :: [String]
      }
  deriving (Show)

instance Arbitrary ValidArgs where
  arbitrary = do
    p <- genLegendPath
    cs <- genCommands
    order <- shuffle (p : cs)
    pure $ ValidArgs p cs order
    where
      genLegendPath = do
        (PrintableString s) <- arbitrary
        pure $ "--legend=" <> s
      genCommands = fmap getPrintableString <$> listOf arbitrary

  shrink (ValidArgs legend commands _) =
    let shrunk = shrink commands
     in fmap (\cs -> ValidArgs legend cs (legend : cs)) shrunk

newtype ValidMapLine = ValidMapLine {unValidMapLine :: (String, String)}
  deriving (Show)

instance Arbitrary ValidMapLine where
  arbitrary = do
    (PrintableString k) <- arbitrary `suchThat` \(PrintableString x) -> validMapLine x
    (PrintableString v) <- arbitrary `suchThat` \(PrintableString x) -> validMapLine x
    pure $ ValidMapLine (k, k <> "=" <> v)
    where
      validMapLine [] = False
      validMapLine ('#' : _) = False
      validMapLine xs = foldr ((&&) . check) True xs
      check '=' = False
      check '\n' = False
      check _ = True

newtype ValidMapLines = ValidMapLines {unValidMapLines :: (Int, [String])}
  deriving (Show)

instance Arbitrary ValidMapLines where
  arbitrary = do
    xs <- listOf $ fmap unValidMapLine arbitrary
    let numUnique = length $ S.fromList $ fmap fst xs
    pure $ ValidMapLines (numUnique, fmap snd xs)

  shrink (ValidMapLines (_, [])) = []
  shrink (ValidMapLines (i, (x:xs))) =
    [ValidMapLines (1, [x]), ValidMapLines (i `seq` i - 1, xs)]

newtype InvalidMapLine = InvalidMapLine String deriving (Show)

instance Arbitrary InvalidMapLine where
  arbitrary = do
    s <- arbitrary `suchThat` \x ->
      nonEmpty x
        && (not . singleEquals) x
        && (not . startsPound) x
    pure $ InvalidMapLine s

newtype InvalidMapLines = InvalidMapLines [String] deriving (Show)

instance Arbitrary InvalidMapLines where
  arbitrary = do
    (ValidMapLines (_, goodLines)) <- arbitrary
    (InvalidMapLine bad) <- arbitrary
    fmap InvalidMapLines $ shuffle (bad : goodLines)

intercalate :: Monoid m => [m] -> m -> m
intercalate [] y = y
intercalate (x : xs) y = x <> y <> intercalate xs y

startsPound :: String -> Bool
startsPound ('#' : _) = True
startsPound _ = False

singleEquals :: String -> Bool
singleEquals xs = f xs False
  where
    f [] b = b
    f ('=' : _) True = False
    f ('=' : ys) False = f ys True
    f (_ : ys) b = f ys b

nonEmpty :: String -> Bool
nonEmpty = (/=) ""
