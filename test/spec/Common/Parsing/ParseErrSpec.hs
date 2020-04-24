module Common.Parsing.ParseErrSpec
  ( spec,
  )
where

import Common.Parsing.ParseErr
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Common.Parsing.ParseErrSpec" $ do
    it "Identity" $ do
      mempty `shouldBe` Err ""
    prop "Is lawful" vLawful
    prop "Help is an ideal" vHelpIdeal
    prop "Err is left biased" vErrLeftBiased
    prop "Help is left biased" vHelpLeftBiased
    prop "Right-lazy when Help is left" vHelpRightLazy

vLawful :: ArbParseErr -> ArbParseErr -> ArbParseErr -> Bool
vLawful (ArbParseErr x) (ArbParseErr y) (ArbParseErr z) =
  mempty <> x == x
    && x <> mempty == x
    && (x <> y) <> z == x <> (y <> z)

vHelpIdeal :: NonEmptyErr -> ArbHelp -> ArbHelp -> Bool
vHelpIdeal (NonEmptyErr e@(Err _)) (ArbHelp h1@(Help _)) (ArbHelp h2@(Help _)) =
  e <> h1 == h1
    && h1 <> e == h1
    && isHelp (h1 <> h2)
    && isHelp (h2 <> h1)
vHelpIdeal _ _ _ = error "vHelpIdeal should only receive (Err, Help, Help)"

vErrLeftBiased :: NonEmptyErr -> NonEmptyErr -> Bool
vErrLeftBiased (NonEmptyErr l) (NonEmptyErr r) = l <> r == l

vHelpLeftBiased :: ArbHelp -> ArbHelp -> Bool
vHelpLeftBiased (ArbHelp l) (ArbHelp r) = l <> r == l

vHelpRightLazy :: ArbHelp -> Bool
vHelpRightLazy (ArbHelp h@(Help _)) = seq (h <> undefined) True
vHelpRightLazy _ = error "vHelpRightLazy should only receive PSuccess"

newtype NonEmptyErr = NonEmptyErr ParseErr deriving (Show)

instance Arbitrary NonEmptyErr where
  arbitrary = do
    s <- arbitrary `suchThat` nonEmpty
    pure $ NonEmptyErr $ Err s

newtype ArbHelp = ArbHelp ParseErr deriving (Show)

instance Arbitrary ArbHelp where
  arbitrary = do
    s <- arbitrary
    pure $ ArbHelp $ Help s

newtype ArbParseErr = ArbParseErr ParseErr deriving (Show)

instance Arbitrary ArbParseErr where
  arbitrary = do
    (NonEmptyErr e) <- arbitrary
    (ArbHelp h) <- arbitrary
    ArbParseErr <$> elements [e, h]

nonEmpty :: String -> Bool
nonEmpty = (/=) ""

isHelp :: ParseErr -> Bool
isHelp (Help _) = True
isHelp _ = False