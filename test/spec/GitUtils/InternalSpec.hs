{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module GitUtils.InternalSpec where

import           Data.Either (isRight)
import qualified Data.Text as Txt
import           Data.Time.Calendar (Day, diffDays)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import GitUtils.Internal
import Types.Error
import Types.GitTypes

import GitUtils.Arbitraries

spec :: Spec
spec = do
  describe "Parsing Tests" $ do
    prop "Correctly formatted name log succeeds" goodLogSucceeds

    prop "Badly formatted name log fails" goodLogSucceeds

    prop "Correctly formatted date string succeeds" goodDateStrSucceeds

    prop "Badly formatted date string fails" badDateStrFails

  describe "Stale tests" $ do
    prop "Log is stale iff diffDays day log > limit " vStale 

goodLogSucceeds :: NameLogSuccess -> Bool
goodLogSucceeds (NameLogSuccess nl@(n, l)) =
  case parseAuthDateStr nl of
      Right (n', Author a, t) -> n' == n && [a, t] == Txt.splitOn "|" l
      _                       -> False

badLogFails :: NameLogErr -> Bool
badLogFails (NameLogErr nl@(_, l)) =
  case parseAuthDateStr nl of
      Left (ParseLog e) -> e == l
      _                 -> False

goodDateStrSucceeds :: NameAuthDateSuccess -> Bool
goodDateStrSucceeds (NameAuthDateSuccess nad) =
  case parseDay nad of
    Right _ -> True
    _       -> False

badDateStrFails :: NameAuthDateErr -> Bool
badDateStrFails (NameAuthDateErr nad@(_, _, t)) =
  case parseDay nad of
    (Left (ParseDate _)) -> goodRead ts
    (Left (ReadInt _))   -> (not . goodRead) ts
    _                    -> False
  where ts = Txt.splitOn "-" t
        goodRead xs = isRight $ traverse safeRead xs

vStale :: Integer -> Day -> NameAuthDay -> Bool
vStale lim day nad@(_, _, d) = (diffDays day d > lim) == isStale
  where isStale = stale lim day nad

newtype NameLogSuccess = NameLogSuccess NameLog
  deriving Show

instance Arbitrary NameLogSuccess where
  arbitrary :: Gen NameLogSuccess
  arbitrary = do
    n <- arbitrary
    l <- genValidLog
    return $ NameLogSuccess (n, l)

newtype NameLogErr = NameLogErr NameLog
  deriving Show

instance Arbitrary NameLogErr where
  arbitrary :: Gen NameLogErr
  arbitrary = do
    n <- arbitrary
    l <- arbitrary
    return $ NameLogErr (n, l)

genValidLog :: Gen Txt.Text
genValidLog = do
  a <- arbitrary `suchThat` Txt.all (/= '|')
  l <- arbitrary `suchThat` Txt.all (/= '|')
  return $ a <> "|" <> l

newtype NameAuthDateSuccess = NameAuthDateSuccess NameAuthDateStr
  deriving Show

instance Arbitrary NameAuthDateSuccess where
  arbitrary :: Gen NameAuthDateSuccess
  arbitrary = do
    n <- arbitrary
    a <- arbitrary
    d <- genValidDateStr
    return $ NameAuthDateSuccess (n, a, d)

newtype NameAuthDateErr = NameAuthDateErr NameAuthDateStr
  deriving Show

instance Arbitrary NameAuthDateErr where
  arbitrary :: Gen NameAuthDateErr
  arbitrary = do
    n <- arbitrary
    a <- arbitrary
    d <- arbitrary
    return $ NameAuthDateErr (n, a, d)

genValidDateStr :: Gen Txt.Text
genValidDateStr = do
  y <- genYearStr
  m <- genMonthStr
  d <- genDayStr
  return $ y <> "-" <> m <> "-" <> d