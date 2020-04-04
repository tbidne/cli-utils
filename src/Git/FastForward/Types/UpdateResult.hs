{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.FastForward.Types.UpdateResult
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides functions and types for descrbing the result of an
-- update attempt.
module Git.FastForward.Types.UpdateResult
  ( SplitResults (..),
    UpdateResult (..),
    displayResults,
    splitResults,
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import Git.Types.GitTypes

-- | Describes the result of an attempt to update a branch.
data UpdateResult
  = -- | Indicates updating 'Name' failed.
    Failure Name
  | -- | Indicates updating 'Name' resulted in no change.
    NoChange Name
  | -- | Indicates updating 'Name' succeeded.
    Success Name
  deriving (Show)

-- | Groups the 'String' 'Name' of all results by 'UpdateResult'.
data SplitResults
  = SplitResults
      { failures :: [String],
        noChanges :: [String],
        successes :: [String]
      }
  deriving (Show)

-- | Summarizes ['UpdateResult'] as a display 'T.Text'.
displayResults :: [UpdateResult] -> T.Text
displayResults = f . splitResults
  where
    f SplitResults {failures, noChanges, successes} =
      "Successes: "
        <> T.pack (show successes)
        <> "\nNo Changes: "
        <> T.pack (show noChanges)
        <> "\nFailures: "
        <> T.pack (show failures)

-- | Maps ['UpdateResult'] to intermediate 'SplitResults'.
splitResults :: [UpdateResult] -> SplitResults
splitResults = F.foldl' f (SplitResults [] [] [])
  where
    f (SplitResults fs ns ss) (Failure (Name x)) = SplitResults (T.unpack x : fs) ns ss
    f (SplitResults fs ns ss) (NoChange (Name x)) = SplitResults fs (T.unpack x : ns) ss
    f (SplitResults fs ns ss) (Success (Name x)) = SplitResults fs ns (T.unpack x : ss)
