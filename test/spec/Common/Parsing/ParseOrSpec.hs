module Common.Parsing.ParseOrSpec
  ( spec,
  )
where

import Common.Parsing.ParseOr
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Common.Parsing.ParseOrSpec" $ do
    it "Identity" $ do
      (mempty :: ParseOr String) `shouldBe` (ParseOr (PFailure (Err "")))
    prop "Is lawful" vLawful
    prop "PSuccess is an ideal" vPSuccessIdeal
    prop "PFailure is left biased" vPFailureLeftBiased
    prop "PSuccess is left biased" vPSuccessLeftBiased
    prop "Right-lazy when PSuccess is left" vPSuccessRightLazy

vLawful :: ArbParseOr -> ArbParseOr -> ArbParseOr -> Bool
vLawful (ArbParseOr x) (ArbParseOr y) (ArbParseOr z) =
  mempty <> x == x
    && x <> mempty == x
    && (x <> y) <> z == x <> (y <> z)

vPSuccessIdeal :: ParseOrFailure -> ParseOrSuccess -> ParseOrSuccess -> Bool
vPSuccessIdeal
  (ParseOrFailure f@(ParseOr (PFailure _)))
  (ParseOrSuccess s1@(ParseOr (PSuccess _)))
  (ParseOrSuccess s2@(ParseOr (PSuccess _))) =
    f <> s1 == s1
      && s1 <> f == s1
      && isPSuccess (s1 <> s2)
      && isPSuccess (s2 <> s1)
vPSuccessIdeal _ _ _ = error "vPSuccessIdeal should only receive (PSuccess, PSuccess)"

vPFailureLeftBiased :: ParseOrFailure -> ParseOrFailure -> Bool
vPFailureLeftBiased
  (ParseOrFailure l@(ParseOr (PFailure _)))
  (ParseOrFailure r@(ParseOr (PFailure _))) =
    l <> r == l
vPFailureLeftBiased _ _ = error "vPFailureLeftBiased should only receive (PFailure, PFailure)"

vPSuccessLeftBiased :: ParseOrSuccess -> ArbParseOr -> Bool
vPSuccessLeftBiased (ParseOrSuccess l@(ParseOr (PSuccess _))) (ArbParseOr r) =
  l <> r == l
vPSuccessLeftBiased _ _ = error "vPSuccessLeftBiased should only receive (PSuccess, _)"

vPSuccessRightLazy :: ParseOrSuccess -> Bool
vPSuccessRightLazy (ParseOrSuccess s@(ParseOr (PSuccess _))) =
  seq (s <> undefined) True
vPSuccessRightLazy _ = error "vPSuccessRightLazy should only receive PSuccess"

newtype ParseOrFailure = ParseOrFailure (ParseOr String) deriving (Show)

instance Arbitrary ParseOrFailure where
  arbitrary = do
    s <- arbitrary `suchThat` nonEmpty
    pure $ ParseOrFailure $ ParseOr $ PFailure $ Err s

newtype ParseOrSuccess = ParseOrSuccess (ParseOr String) deriving (Show)

instance Arbitrary ParseOrSuccess where
  arbitrary = do
    s <- arbitrary `suchThat` nonEmpty
    pure $ ParseOrSuccess $ ParseOr $ PSuccess s

newtype ArbParseOr = ArbParseOr (ParseOr String) deriving (Show)

instance Arbitrary ArbParseOr where
  arbitrary = do
    (ParseOrFailure f) <- arbitrary
    (ParseOrSuccess s) <- arbitrary
    ArbParseOr <$> elements [f, s]

nonEmpty :: String -> Bool
nonEmpty = (/=) ""

isPSuccess :: ParseOr a -> Bool
isPSuccess (ParseOr (PSuccess _)) = True
isPSuccess _ = False
