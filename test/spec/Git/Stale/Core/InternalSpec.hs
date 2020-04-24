{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Stale.Core.InternalSpec
  ( spec,
  )
where

import Common.ArbNonNegative
import Common.Types.NonNegative
import qualified Data.Either as E
import qualified Data.Text as Txt
import qualified Data.Time.Calendar as C
import Git.Stale.Core.Arbitraries
import Git.Stale.Core.Internal
import Git.Stale.Types.Arbitraries ()
import Git.Stale.Types.Error
import Git.Types.GitTypes
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as Q

spec :: Spec
spec = do
  describe "Git.Stale.Core.InternalSpec" $ do
    prop "Correctly formatted name log succeeds" goodLogSucceeds
    prop "Badly formatted name log fails" badLogFails
    prop "Correctly formatted date string succeeds" goodDateStrSucceeds
    prop "Badly formatted date string fails" badDateStrFails
  describe "Stale tests" $ do
    prop "Log is stale iff diffDays day log >= limit " vStale

goodLogSucceeds :: NameLogSuccess -> Bool
goodLogSucceeds (NameLogSuccess nl@(n, l)) = case parseAuthDateStr nl of
  Right (n', Author a, t) -> n' == n && [a, t] == Txt.splitOn "|" l
  _ -> False

badLogFails :: NameLogErr -> Bool
badLogFails (NameLogErr nl@(_, l)) = case parseAuthDateStr nl of
  Left (ParseLog e) -> e == l
  _ -> False

goodDateStrSucceeds :: NameAuthDateSuccess -> Bool
goodDateStrSucceeds (NameAuthDateSuccess nad) = case parseDay nad of
  Right _ -> True
  _ -> False

badDateStrFails :: NameAuthDateErr -> Bool
badDateStrFails (NameAuthDateErr nad@(_, _, t)) = case parseDay nad of
  (Left (ParseDate _)) -> goodRead ts
  (Left (ReadInt _)) -> (not . goodRead) ts
  _ -> False
  where
    ts = Txt.splitOn "-" t
    goodRead xs = E.isRight $ traverse safeRead xs

vStale :: NonNegative Int -> C.Day -> NameAuthDay -> Bool
vStale lim day nad@(_, _, d) = (C.diffDays day d > (fromIntegral (getNonNegative lim))) == isStale
  where
    isStale = stale lim day nad

newtype NameLogSuccess = NameLogSuccess NameLog
  deriving (Show)

instance Q.Arbitrary NameLogSuccess where
  arbitrary :: Q.Gen NameLogSuccess
  arbitrary = do
    n <- Q.arbitrary
    l <- genValidLog
    pure $ NameLogSuccess (n, l)

newtype NameLogErr = NameLogErr NameLog
  deriving (Show)

instance Q.Arbitrary NameLogErr where
  arbitrary :: Q.Gen NameLogErr
  arbitrary = do
    n <- Q.arbitrary `Q.suchThat` (\(Name n) -> Txt.all (/= '|') n)
    l <- Q.arbitrary `Q.suchThat` Txt.all (/= '|')
    pure $ NameLogErr (n, l)

genValidLog :: Q.Gen Txt.Text
genValidLog = do
  a <- Q.arbitrary `Q.suchThat` Txt.all (/= '|')
  l <- Q.arbitrary `Q.suchThat` Txt.all (/= '|')
  pure $ a <> "|" <> l

newtype NameAuthDateSuccess = NameAuthDateSuccess NameAuthDateStr
  deriving (Show)

instance Q.Arbitrary NameAuthDateSuccess where
  arbitrary :: Q.Gen NameAuthDateSuccess
  arbitrary = do
    n <- Q.arbitrary
    a <- Q.arbitrary
    d <- genValidDateStr
    pure $ NameAuthDateSuccess (n, a, d)

newtype NameAuthDateErr = NameAuthDateErr NameAuthDateStr
  deriving (Show)

instance Q.Arbitrary NameAuthDateErr where
  arbitrary :: Q.Gen NameAuthDateErr
  arbitrary = do
    n <- Q.arbitrary
    a <- Q.arbitrary
    d <- Q.arbitrary
    pure $ NameAuthDateErr (n, a, d)

genValidDateStr :: Q.Gen Txt.Text
genValidDateStr = do
  y <- genYearStr
  m <- genMonthStr
  d <- genDayStr
  pure $ y <> "-" <> m <> "-" <> d
