{-# LANGUAGE OverloadedStrings #-}

module Git.Stale.Types.BranchSpec
  ( spec,
  )
where

import Data.Time.Calendar (Day)
import Git.Stale.Core.Arbitraries ()
import Git.Stale.Types.Branch
import Git.Types.GitTypes
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Branch Tests" $ do
    prop "Verifying branch cons" vBranch

vBranch :: Name -> Author -> Day -> Bool -> Bool
vBranch n a d b = case mkAnyBranch n a d b of
  MergedBranch _ -> b
  UnMergedBranch _ -> not b
