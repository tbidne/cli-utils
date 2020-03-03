{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Arbitraries
( Arbitrary(..)
, genYear
, genYearStr
, genMonth
, genMonthStr
, genDay
, genDayStr
) where

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
genYear = read . Txt.unpack <$> genYearStr

genYearStr :: Gen Txt.Text
genYearStr = fmap Txt.pack $ vectorOf 4 $ elements ['0'..'9']

genMonth :: Gen Int
genMonth = read . Txt.unpack <$> genMonthStr

genMonthStr :: Gen Txt.Text
genMonthStr = 
  let ms = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]
  in elements ms

genDay :: Gen Int
genDay = read . Txt.unpack <$> genDayStr

genDayStr :: Gen Txt.Text
genDayStr = do
  a <- elements ['0'..'2']
  b <- elements ['0'..'8']
  return $ Txt.pack $ [a, b]

instance Arbitrary Day where
  arbitrary :: Gen Day
  arbitrary = do
      y <- genYear
      d <- genDay
      return $ fromJulianYearAndDay y d