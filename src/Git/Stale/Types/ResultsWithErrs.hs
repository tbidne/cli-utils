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

-- | Newtype wrapper for errors over (display string, num branches).
newtype ErrDisp = ErrDisp (T.Text, Int)

-- | Newtype wrapper over ('ErrDisp', 'ResultsDisp').
newtype ResultsWithErrsDisp = ResultsWithErrsDisp (ErrDisp, ResultsDisp)

-- | Transforms the 'ResultsWithErrs' into a 'ResultsWithErrsDisp' for display
-- purposes. Strips out the @prefix@ from the branches, if it exists.
toResultsErrDisp :: T.Text -> ResultsWithErrs -> ResultsWithErrsDisp
toResultsErrDisp prefix ResultsWithErrs {errList, results} = ResultsWithErrsDisp (errDisp, res)
  where
    errDisp = ErrDisp (showText errList, length errList)
    res = toResultsDisp prefix results

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
