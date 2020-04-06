{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.InternalSpec where

import Git.FastForward.Core.Arbitraries
import Git.FastForward.Core.Internal
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "FastForward Internal Tests" $ do
    prop "Correctly formatted branches are parsed" validBranchesAreParsed
    prop "Missing starred (current) branch dies" noStarDies
    it "Up to date branch tests" $ do
      "Already up to date.\n" `shouldSatisfy` branchUpToDate
      "" `shouldSatisfy` not . branchUpToDate
      "Everything up-to-date\n" `shouldSatisfy` remoteUpToDate
      "" `shouldSatisfy` not . remoteUpToDate

validBranchesAreParsed :: ValidLocalBranches -> Bool
validBranchesAreParsed (ValidLocalBranches txt) =
  case textToLocalBranches txt of
    Right _ -> True
    Left _ -> False

noStarDies :: BranchesNoStar -> Bool
noStarDies (BranchesNoStar txt) =
  case textToLocalBranches txt of
    Right _ -> False
    Left _ -> True
