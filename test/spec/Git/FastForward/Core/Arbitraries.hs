{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.Arbitraries
  ( ValidLocalBranches (..),
    BranchesNoStar (..),
    StarredBranch (..),
  )
where

import qualified Data.Text as T
import Test.QuickCheck

newtype ValidLocalBranches = ValidLocalBranches T.Text
  deriving (Show)

instance Arbitrary ValidLocalBranches where
  arbitrary = do
    names <- listOf (arbitrary `suchThat` noStar)
    (StarredBranch starred) <- arbitrary
    pure $ ValidLocalBranches $
      starred
        <> "\n"
        <> foldMap (\(PrintableString s) -> (T.pack s) <> "\n") names

newtype BranchesNoStar = BranchesNoStar T.Text
  deriving (Show)

instance Arbitrary BranchesNoStar where
  arbitrary = do
    names <- listOf (arbitrary `suchThat` noStar)
    pure $ BranchesNoStar $
      foldMap (\(PrintableString s) -> (T.pack s) <> "\n") names

newtype StarredBranch = StarredBranch T.Text
  deriving (Show)

instance Arbitrary StarredBranch where
  arbitrary = do
    (PrintableString s) <- arbitrary `suchThat` \x -> noStar x && nonEmpty x
    pure $ StarredBranch ("* " <> (T.pack s))

nonEmpty :: PrintableString -> Bool
nonEmpty = (/=) "" . getPrintableString

noStar :: PrintableString -> Bool
noStar = not . any (== '*') . getPrintableString
