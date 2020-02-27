{-# LANGUAGE InstanceSigs #-}

module Arbitraries (Arbitrary(..)) where

import           Data.Time.Calendar (Day)
import           Data.Time.Calendar.Julian (fromJulianYearAndDay)
import qualified Data.Text as Txt
import           Test.QuickCheck

import Types.GitTypes

instance Arbitrary Txt.Text where
  arbitrary :: Gen Txt.Text
  arbitrary = fmap (Txt.pack . getPrintableString) (arbitrary :: Gen PrintableString)

instance Arbitrary Name where
  arbitrary :: Gen Name
  arbitrary = fmap Name arbitrary

instance Arbitrary Author where
  arbitrary :: Gen Author
  arbitrary = fmap Author arbitrary

genYear :: Gen Integer
genYear = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (\x -> x > 2000 && x < 2020)

genDay :: Gen Int
genDay = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x > 0 && x < 365)

instance Arbitrary Day where
  arbitrary :: Gen Day
  arbitrary = do
      y <- genYear
      d <- genDay
      return $ fromJulianYearAndDay y d