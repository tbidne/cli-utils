{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Types.ResultsWithErrs
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides a `ResultsWithErrs` type that wraps `Results` along with
-- a list of errors.
module Types.ResultsWithErrs
  ( ResultsWithErrs (..),
    displayResultsWithErrs,
    toResultsWithErrs,
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import Types.Branch
import Types.Error
import Types.Results

-- | Wraps `Results` and includes [`Err`].
data ResultsWithErrs
  = ResultsWithErrs
      { -- | All errors encountered.
        errList :: [Err],
        -- | The `Results`.
        results :: Results
      }
  deriving (Show)

-- | Displays `ResultsWithErrs`. Differs from `Show` in that it is formatted differently
-- and strips the `T.Text` /prefix/ from the branch names.
displayResultsWithErrs :: T.Text -> ResultsWithErrs -> T.Text
displayResultsWithErrs prefix ResultsWithErrs {errList, results} =
  T.concat str
  where
    str = ["ERRORS\n------\n", T.pack (show errList), "\n\n", displayResults prefix results]

-- | Maps [`ErrOr` `AnyBranch`] to `ResultsWithErrs`.
toResultsWithErrs :: [ErrOr AnyBranch] -> ResultsWithErrs
toResultsWithErrs xs = ResultsWithErrs errs (toResults results)
  where
    (errs, results) = splitErrs xs

splitErrs :: [ErrOr AnyBranch] -> ([Err], [AnyBranch])
splitErrs = F.foldl' f ([], [])
  where
    f (es, bs) (Left e) = (e : es, bs)
    f (es, bs) (Right b) = (es, b : bs)
