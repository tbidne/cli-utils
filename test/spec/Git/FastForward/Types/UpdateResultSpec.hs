{-# LANGUAGE NamedFieldPuns #-}

module Git.FastForward.Types.UpdateResultSpec
  ( spec,
  )
where

import Git.FastForward.Types.Arbitraries ()
import Git.FastForward.Types.UpdateResult
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Git.FastForward.Types.UpdateResultSpec" $ do
    prop "Splits UpdateResults correctly" verifySplitsResults

verifySplitsResults :: [UpdateResult] -> Bool
verifySplitsResults rs =
  let SplitResults {failures, noChanges, successes} = splitResults rs
   in length rs
        == length failures
        + length noChanges
        + length successes
