{-# LANGUAGE InstanceSigs #-}

module Git.Stale.Types.Arbitraries
  ( Arbitrary (..),
  )
where

import Git.Stale.Core.Arbitraries ()
import Git.Stale.Types.Branch
import Git.Stale.Types.Env
import Git.Stale.Types.Error
import Test.QuickCheck

instance Arbitrary Err where
  arbitrary :: Gen Err
  arbitrary = do
    e <- arbitrary
    elements
      [ ParseLog e,
        ParseDate e,
        ParseMerge e,
        ReadInt e,
        GitBranches e,
        GitLog e
      ]

instance Arbitrary BranchStatus where
  arbitrary :: Gen BranchStatus
  arbitrary = elements [Merged, UnMerged]

instance Arbitrary AnyBranch where
  arbitrary :: Gen AnyBranch
  arbitrary = do
    n <- arbitrary
    a <- arbitrary
    d <- arbitrary
    b <- arbitrary
    pure $ mkAnyBranch n a d b

instance Arbitrary BranchType where
  arbitrary :: Gen BranchType
  arbitrary = elements [All, Remote, Local]