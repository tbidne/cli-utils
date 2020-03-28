{-# LANGUAGE NamedFieldPuns #-}

module Git.FastForward.Types.UpdateResultSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Git.FastForward.Types.Arbitraries ()
import Git.FastForward.Types.UpdateResult

spec :: Spec
spec = do
  describe "UpdateResult tests" $ do
    prop "Splits UpdateResults correctly" verifySplitsResults

verifySplitsResults :: [UpdateResult] -> Bool
verifySplitsResults rs =
  let SplitResults {failures, noChanges, successes} = splitResults rs
  in length rs ==
    length failures
    + length noChanges
    + length successes