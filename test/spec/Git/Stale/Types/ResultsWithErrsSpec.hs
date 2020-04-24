module Git.Stale.Types.ResultsWithErrsSpec
  ( spec,
  )
where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Git.Stale.Types.Arbitraries ()
import Git.Stale.Types.Branch
import Git.Stale.Types.Error
import Git.Stale.Types.Results
import Git.Stale.Types.ResultsWithErrs
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Git.Stale.Types.ResultsWithErrsSpec" $ do
    prop
      "size(unique_branches) should equal size(merge_map) + size(unmerged_map) + size(errs)"
      sizesMatch

sizesMatch :: [ErrOr AnyBranch] -> Bool
sizesMatch bs = numUnique bs == M.size merged + M.size unmerged + length errs
  where
    resultsErrs = toResultsWithErrs bs
    merged = mergedMap $ results resultsErrs
    unmerged = unMergedMap $ results resultsErrs
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
