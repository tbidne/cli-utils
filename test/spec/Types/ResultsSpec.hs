module Types.ResultsSpec where

import           Data.Foldable (foldl')
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import Types.Branch

import Types.Arbitraries()
import Types.Results

spec :: Spec
spec = do
  describe "Results tests" $ do
    prop "size(unique_branches) should equal size(merge_map) + size(unmerged_map)" sizesMatch

sizesMatch :: [AnyBranch] -> Bool
sizesMatch bs = numUnique bs == Map.size merged + Map.size unmerged
  where results  = toResults bs
        merged   = mergedMap results
        unmerged = unmergedMap results

numUnique :: [AnyBranch] -> Int
numUnique = pairSetLen . foldl' f (Set.empty, Set.empty)
  where f (mSet, umSet) (MergedBranch   (MkBranch _ a _)) = (Set.insert a mSet, umSet)
        f (mSet, umSet) (UnMergedBranch (MkBranch _ a _)) = (mSet, Set.insert a umSet)

pairSetLen :: (Set.Set a, Set.Set b) -> Int
pairSetLen (a, b) = Set.size a + Set.size b