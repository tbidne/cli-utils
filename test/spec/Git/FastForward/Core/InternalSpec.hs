module Git.FastForward.Core.InternalSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Git.FastForward.Core.Internal
import Git.FastForward.Core.Arbitraries

spec :: Spec
spec = do
  describe "FastForward Internal Tests" $ do
    prop "Correctly formatted branches are parsed" validBranchesAreParsed
    prop "Missing starred (current) branch dies" noStarDies
    prop "Already updated branch handled" alreadyUpdatedTrue
    prop "Not already updated branch handled" notAlreadyUpdatedFalse

validBranchesAreParsed :: ValidLocalBranches -> Bool
validBranchesAreParsed (ValidLocalBranches txt) =
  case textToLocalBranches txt of
    Just _ -> True
    Nothing -> False

noStarDies :: BranchesNoStar -> Bool
noStarDies (BranchesNoStar txt) =
  case textToLocalBranches txt of
    Just _ -> False
    Nothing -> True

alreadyUpdatedTrue :: AlreadyUpdated -> Bool
alreadyUpdatedTrue (AlreadyUpdated s) = alreadyUpdated s

notAlreadyUpdatedFalse :: NotAlreadyUpdated -> Bool
notAlreadyUpdatedFalse (NotAlreadyUpdated s) = not (alreadyUpdated s)