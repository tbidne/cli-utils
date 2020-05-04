{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.UtilsSpec
  ( spec,
  )
where

import qualified Common.RefinedUtils as R
import Common.Utils
import Control.Applicative ((<|>))
import qualified Data.Maybe as May
import qualified System.Clock as Clock
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Common.UtilsSpec" $ do
    prop "startsWith and matchAndStrip should return Just" validStartsWith
    it "startsWith and matchAndStrip should return Nothing" $ do
      ("a", "") `shouldSatisfy` invalidStartsWith
      ("a", "ba") `shouldSatisfy` invalidStartsWith
    it "diffTime should return absolute time diff" $ do
      let s1 = Clock.fromNanoSecs 5_000_000_000
      let s2 = Clock.fromNanoSecs 10_000_000_000
      diffTime s1 s2 `shouldSatisfy` ((==) 5) . R.unrefine
      diffTime s2 s1 `shouldSatisfy` ((==) 5) . R.unrefine
      diffTime s1 s1 `shouldSatisfy` ((==) 0) . R.unrefine
    prop "divWithRem n d, d <= n should be (e, r) s.t. de + r = n" vDivWithRem
    it "formatSeconds should format the seconds" $ do
      formatSeconds (R.unsafeNonNeg 0) `shouldBe` "0 minutes and 0 seconds  "
      formatSeconds (R.unsafeNonNeg 30) `shouldBe` "0 minutes and 30 seconds  "
      formatSeconds (R.unsafeNonNeg 60) `shouldBe` "1 minute and 0 seconds  "
      formatSeconds (R.unsafeNonNeg 301) `shouldBe` "5 minutes and 1 second  "

validStartsWith :: ValidStartsWith -> Bool
validStartsWith (ValidStartsWith (prefix, rest)) =
  May.isJust $
    sequenceA
      [ rest `startsWith` prefix,
        matchAndStrip prefix rest
      ]

invalidStartsWith :: (String, String) -> Bool
invalidStartsWith (prefix, rest) =
  May.isNothing $
    (rest `startsWith` prefix) <|> (matchAndStrip prefix rest)

vDivWithRem :: NatAndPosNonZero -> Bool
vDivWithRem (NatAndPosNonZero (n, d)) =
  let (e, r) = divWithRem n d
   in ((R.unrefine d) * e) + r == (R.unrefine n)
        && r <= (R.unrefine n)

newtype ValidStartsWith = ValidStartsWith (String, String) deriving (Show)

instance Arbitrary ValidStartsWith where
  arbitrary = do
    prefix <- arbitrary
    rest <- arbitrary
    pure $ ValidStartsWith (prefix, prefix <> rest)

newtype NatAndPosNonZero = NatAndPosNonZero (R.RNonNegative Int, R.RPositive Int)
  deriving (Show)

instance Arbitrary NatAndPosNonZero where
  arbitrary = do
    (NonNegative n) <- arbitrary :: Gen (NonNegative Int)
    (Positive d) <- arbitrary :: Gen (Positive Int)
    pure $ NatAndPosNonZero (R.unsafeNonNeg n, R.unsafePos d)
