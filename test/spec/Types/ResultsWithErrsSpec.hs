module Types.ResultsWithErrsSpec where

import           Data.Foldable (foldl')
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Test.Hspec.QuickCheck

import Types.Branch
import Types.Error
import Types.Results
import Types.ResultsWithErrs

import Types.Arbitraries()

spec :: Spec
spec = do
  describe "ResultsWithErrs tests" $ do
    prop "size(unique_branches) should equal size(merge_map) + size(unmerged_map) + size(errs)" sizesMatch

sizesMatch :: [Either Err AnyBranch] -> Bool
sizesMatch bs = numUnique bs == Map.size merged + Map.size unmerged + length errs
  where resultsErrs  = toResultsWithErrs bs
        merged   = mergedMap   $ results resultsErrs
        unmerged = unmergedMap $ results resultsErrs
        errs     = errList resultsErrs

numUnique :: [Either Err AnyBranch] -> Int
numUnique bs = (setsLen . foldl' f (0, Set.empty, Set.empty)) bs
  where f (errs, m, u) (Left _)                                  = (errs + 1, m, u)
        f (errs, m, u) (Right (MergedBranch   (MkBranch _ a _))) = (errs, Set.insert a m, u)
        f (errs, m, u) (Right (UnMergedBranch (MkBranch _ a _))) = (errs, m, Set.insert a u)

setsLen :: (Int, Set.Set a, Set.Set b) -> Int
setsLen (errs, a, b) = errs + Set.size a + Set.size b