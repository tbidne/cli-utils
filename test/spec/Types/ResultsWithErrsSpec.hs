module Types.ResultsWithErrsSpec where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec
import Test.Hspec.QuickCheck
import Types.Arbitraries ()
import Types.Branch
import Types.Error
import Types.Results
import Types.ResultsWithErrs

spec :: Spec
spec = do
  describe "ResultsWithErrs tests" $ do
    prop
      "size(unique_branches) should equal size(merge_map) + size(unmerged_map) + size(errs)"
      sizesMatch

sizesMatch :: [ErrOr AnyBranch] -> Bool
sizesMatch bs = numUnique bs == M.size merged + M.size unmerged + length errs
  where
    resultsErrs = toResultsWithErrs bs
    merged = mergedMap $ results resultsErrs
    unmerged = unmergedMap $ results resultsErrs
    errs = errList resultsErrs

numUnique :: [ErrOr AnyBranch] -> Int
numUnique bs = (setsLen . foldl' f (0, S.empty, S.empty)) bs
  where
    f (errs, m, u) (Left _) = (errs + 1, m, u)
    f (errs, m, u) (Right (MergedBranch (MkBranch _ a _))) =
      (errs, S.insert a m, u)
    f (errs, m, u) (Right (UnMergedBranch (MkBranch _ a _))) =
      (errs, m, S.insert a u)

setsLen :: (Int, S.Set a, S.Set b) -> Int
setsLen (errs, a, b) = errs + S.size a + S.size b
