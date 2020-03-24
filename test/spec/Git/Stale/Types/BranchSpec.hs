{-# LANGUAGE OverloadedStrings #-}

module Git.Stale.Types.BranchSpec
  ( spec,
  )
where

import Git.Stale.Core.Arbitraries ()
import Data.Time.Calendar (Day)
import Test.Hspec
import Test.Hspec.QuickCheck
import Git.Stale.Types.Branch
import Git.Types.GitTypes

spec :: Spec
spec = do
  describe "Branch Tests" $ do
    prop "Verifying branch cons" vBranch

vBranch :: Name -> Author -> Day -> Bool -> Bool
vBranch n a d b = case mkAnyBranch n a d b of
  MergedBranch _ -> b
  UnMergedBranch _ -> not b
