{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.UtilsSpec
  ( spec,
  )
where

import Common.ArbNonNegative ()
import Common.ArbPositive ()
import Common.Types.NonNegative
import Common.Types.Positive
import Common.Utils
import Control.Applicative ((<|>))
import qualified Data.Maybe as May
import qualified System.Clock as Clock
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as Q

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
      diffTime s1 s2 `shouldSatisfy` ((==) 5) . (getNonNegative :: NonNegative Int -> Int)
      diffTime s2 s1 `shouldSatisfy` ((==) 5) . (getNonNegative :: NonNegative Int -> Int)
      diffTime s1 s1 `shouldSatisfy` ((==) 0) . (getNonNegative :: NonNegative Int -> Int)
    prop "divWithRem n d should be (e, r) s.t. de + r = n, r <= n" vDivWithRem
    it "formatSeconds should format the seconds" $ do
      formatSeconds (May.fromJust (iToNonNegative 0))
        `shouldBe` "0 minutes and 0 seconds  "
      formatSeconds (May.fromJust (iToNonNegative 30))
        `shouldBe` "0 minutes and 30 seconds  "
      formatSeconds (May.fromJust (iToNonNegative 60))
        `shouldBe` "1 minute and 0 seconds  "
      formatSeconds (May.fromJust (iToNonNegative 301))
        `shouldBe` "5 minutes and 1 second  "

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
   in ((getPositive d) * e) + r == (getNonNegative n)
        && r <= getNonNegative n

newtype ValidStartsWith = ValidStartsWith (String, String) deriving (Show)

instance Q.Arbitrary ValidStartsWith where
  arbitrary = do
    prefix <- Q.arbitrary
    rest <- Q.arbitrary
    pure $ ValidStartsWith (prefix, prefix <> rest)

newtype NatAndPosNonZero = NatAndPosNonZero (NonNegative Int, Positive Int)
  deriving (Show)

instance Q.Arbitrary NatAndPosNonZero where
  arbitrary = do
    n <- Q.arbitrary
    d <- Q.arbitrary
    pure $ NatAndPosNonZero (n, d)
