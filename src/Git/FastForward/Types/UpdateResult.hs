{-# LANGUAGE NamedFieldPuns #-}

module Git.FastForward.Types.UpdateResult
  ( UpdateResult (..),
    displayResults,
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import Git.Types.GitTypes

data UpdateResult
  = Failure Name
  | NoChange Name
  | Success Name
  deriving (Show)

data SplitResults
  = SplitResults
      { failures :: [String],
        noChanges :: [String],
        successes :: [String]
      }
  deriving (Show)

displayResults :: [UpdateResult] -> String
displayResults = f . splitResults
  where
    f SplitResults {failures, noChanges, successes} =
      "***** SUMMARY *****"
        <> "\nFailures: "
        <> show failures
        <> "\nNo Changes: "
        <> show noChanges
        <> "\nSuccesses: "
        <> show successes
        <> "\n*******************"

splitResults :: [UpdateResult] -> SplitResults
splitResults = F.foldl' f (SplitResults [] [] [])
  where
    f (SplitResults fs ns ss) (Failure (Name x)) = SplitResults (T.unpack x : fs) ns ss
    f (SplitResults fs ns ss) (NoChange (Name x)) = SplitResults fs (T.unpack x : ns) ss
    f (SplitResults fs ns ss) (Success (Name x)) = SplitResults fs ns (T.unpack x : ss)
