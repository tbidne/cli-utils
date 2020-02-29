{-# LANGUAGE InstanceSigs #-}

module Types.Arbitraries
( Arbitrary(..)
) where

import           Test.QuickCheck

import Types.Branch
import Types.Error

import GitUtils.Arbitraries()

instance Arbitrary Err where
  arbitrary :: Gen Err
  arbitrary = do
    e <- arbitrary
    elements [ ParseLog e
             , ParseDate e
             , ParseMerge e
             , ReadInt e
             , GitBranches e
             , GitLog e ]

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
    return $ mkAnyBranch n a d b