module Common.Parsing.ParseAndSpec
  ( spec,
  )
where

import Common.Parsing.ParseAnd
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Common.Parsing.ParseAnd" $ do
    it "Identity" $ do
      mempty `shouldBe` (ParseAnd (PSuccess ""))
    prop "Is lawful" vLawful
    prop "PFailure is an ideal" vPFailureIdeal
    prop "PFailure is left biased" vPFailureLeftBiased
    prop "PSuccess is combined" vPSuccessCombine
    prop "Right-lazy when PFailure is left" vPFailureRightLazy

vLawful :: ArbParseAnd -> ArbParseAnd -> ArbParseAnd -> Bool
vLawful (ArbParseAnd x) (ArbParseAnd y) (ArbParseAnd z) =
  mempty <> x == x
    && x <> mempty == x
    && (x <> y) <> z == x <> (y <> z)

vPFailureIdeal :: ParseAndSuccess -> ParseAndFailure -> ParseAndFailure -> Bool
vPFailureIdeal
  (ParseAndSuccess s@(ParseAnd (PSuccess _)))
  (ParseAndFailure f1@(ParseAnd (PFailure _)))
  (ParseAndFailure f2@(ParseAnd (PFailure _))) =
    s <> f1 == f1
      && f1 <> s == f1
      && isPFailure (f1 <> f2)
      && isPFailure (f2 <> f1)
vPFailureIdeal _ _ _ = error "vPFailureIdeal should only receive (PSuccess, PFailure, PFailure)"

vPFailureLeftBiased :: ParseAndFailure -> ArbParseAnd -> Bool
vPFailureLeftBiased
  (ParseAndFailure l@(ParseAnd (PFailure _)))
  (ArbParseAnd r) =
    l <> r == l
vPFailureLeftBiased _ _ = error "vPFailureLeftBiased should only receive (PFailure, _)"

vPSuccessCombine :: ParseAndSuccess -> ParseAndSuccess -> Bool
vPSuccessCombine
  (ParseAndSuccess l@(ParseAnd (PSuccess l')))
  (ParseAndSuccess r@(ParseAnd (PSuccess r'))) =
    l <> r == (ParseAnd (PSuccess (l' <> r')))
vPSuccessCombine _ _ = error "vPSuccessCombine should only receive (PSuccess, PSuccess)"

vPFailureRightLazy :: ParseAndFailure -> Bool
vPFailureRightLazy (ParseAndFailure l@(ParseAnd (PFailure _))) =
  seq (l <> undefined) True
vPFailureRightLazy _ = error "vPFailureRightLazy should only receive PFailure"

newtype ParseAndFailure = ParseAndFailure (ParseAnd String) deriving (Show)

instance Arbitrary ParseAndFailure where
  arbitrary = do
    s <- arbitrary `suchThat` nonEmpty
    pure $ ParseAndFailure $ ParseAnd $ PFailure $ Err s

newtype ParseAndSuccess = ParseAndSuccess (ParseAnd String) deriving (Show)

instance Arbitrary ParseAndSuccess where
  arbitrary = do
    s <- arbitrary `suchThat` nonEmpty
    pure $ ParseAndSuccess $ ParseAnd $ PSuccess s

newtype ArbParseAnd = ArbParseAnd (ParseAnd String) deriving (Show)

instance Arbitrary ArbParseAnd where
  arbitrary = do
    (ParseAndFailure f) <- arbitrary
    (ParseAndSuccess s) <- arbitrary
    ArbParseAnd <$> elements [f, s]

nonEmpty :: String -> Bool
nonEmpty = (/=) ""

isPFailure :: ParseAnd a -> Bool
isPFailure (ParseAnd (PFailure _)) = True
isPFailure _ = False
