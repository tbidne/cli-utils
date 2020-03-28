{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.Arbitraries where

import Test.QuickCheck
import qualified Data.Text as T

newtype ValidLocalBranches = ValidLocalBranches T.Text
  deriving Show

instance Arbitrary ValidLocalBranches where
  arbitrary = do
    names <- listOf (arbitrary `suchThat` noStar)
    (StarredBranch starred) <- arbitrary
    pure $ ValidLocalBranches $
      starred
        <> "\n"
        <> foldMap (\(PrintableString s) -> (T.pack s) <> "\n") names

newtype BranchesNoStar = BranchesNoStar T.Text
  deriving Show

instance Arbitrary BranchesNoStar where
  arbitrary = do
    names <- listOf (arbitrary `suchThat` noStar)
    pure $ BranchesNoStar $
      foldMap (\(PrintableString s) -> (T.pack s) <> "\n") names

newtype StarredBranch = StarredBranch T.Text
  deriving Show

instance Arbitrary StarredBranch where
  arbitrary = do
    (PrintableString s) <- arbitrary `suchThat` \x -> noStar x && nonEmpty x
    pure $ StarredBranch ("* " <> (T.pack s))

newtype AlreadyUpdated = AlreadyUpdated T.Text
  deriving Show

instance Arbitrary AlreadyUpdated where
  arbitrary = do
    (PrintableString prefix) <- arbitrary
    (PrintableString  suffix) <- arbitrary
    pure $ AlreadyUpdated $ T.pack $ prefix <> "Already up to date" <> suffix

newtype NotAlreadyUpdated = NotAlreadyUpdated T.Text
  deriving Show

instance Arbitrary NotAlreadyUpdated where
  arbitrary = do
    (PrintableString s) <- arbitrary
    pure $ NotAlreadyUpdated $ T.pack s

nonEmpty :: PrintableString -> Bool
nonEmpty = (/=) "" . getPrintableString

noStar :: PrintableString -> Bool
noStar = not . any (== '*') . getPrintableString