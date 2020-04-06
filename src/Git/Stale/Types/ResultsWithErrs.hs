{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.Stale.Types.ResultsWithErrs
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides a `ResultsWithErrs` type that wraps `Results` along with
-- a list of errors.
module Git.Stale.Types.ResultsWithErrs
  ( ErrDisp (..),
    ResultsWithErrs (..),
    ResultsWithErrsDisp (..),
    displayResultsWithErrs,
    toResultsErrDisp,
    toResultsWithErrs,
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import Git.Stale.Types.Branch
import Git.Stale.Types.Error
import Git.Stale.Types.Results

-- | Wraps `Results` and includes [`Err`].
data ResultsWithErrs
  = ResultsWithErrs
      { -- | All errors encountered.
        errList :: [Err],
        -- | The `Results`.
        results :: Results
      }
  deriving (Show)

newtype ErrDisp = ErrDisp (T.Text, Int)

newtype ResultsWithErrsDisp = ResultsWithErrsDisp (ErrDisp, ResultsDisp)

toResultsErrDisp :: T.Text -> ResultsWithErrs -> ResultsWithErrsDisp
toResultsErrDisp prefix ResultsWithErrs {errList, results} = ResultsWithErrsDisp (errDisp, res)
  where
    errDisp = ErrDisp (showText errList, length errList)
    res = toResultsDisp prefix results

-- | Displays `ResultsWithErrs`. Differs from `Show` in that it is formatted differently
-- and strips the `T.Text` /prefix/ from the branch names.
displayResultsWithErrs :: T.Text -> ResultsWithErrs -> T.Text
displayResultsWithErrs prefix ResultsWithErrs {errList, results} =
  T.concat str
  where
    str =
      [ "ERRORS: ",
        showText (length errList),
        "\n------\n",
        showText errList,
        "\n\n",
        displayResults prefix results
      ]

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

showText :: Show a => a -> T.Text
showText = T.pack . show
