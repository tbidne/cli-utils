{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module ResultsWithErrs
( ResultsWithErrs
, toResultsWithErrs
) where

import Data.Foldable (foldl')

import Branch
import Error
import Results

data ResultsWithErrs = ResultsWithErrs
  { errList :: [Err]
  , results :: Results
  }

instance Show ResultsWithErrs where
  show ResultsWithErrs{..} = concat str
    where str = ["ERRORS\n------\n", show errList, "\n\n", show results]

toResultsWithErrs :: [Either Err AnyBranch] -> ResultsWithErrs
toResultsWithErrs xs = ResultsWithErrs errs (toResults results)
  where (errs, results) = splitErrs xs

splitErrs :: [Either Err AnyBranch] -> ([Err], [AnyBranch])
splitErrs = foldl' f ([], [])
  where f (es, bs) (Left e)  = (e:es, bs)
        f (es, bs) (Right b) = (es, b:bs)