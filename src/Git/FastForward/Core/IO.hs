{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.FastForward.Core.IO
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports functions to be used by "Git.FastForward.Core.UpdateBranches" for `IO`.
module Git.FastForward.Core.IO
  ( checkoutCurrentIO,
    fetchIO,
    getBranchesIO,
    pushBranchIO,
    summarizeIO,
    updateBranchIO,
  )
where

import Common.IO
import qualified Data.Text as T
import Git.FastForward.Core.Internal
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.MergeType
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes
import qualified System.Exit as Ex

-- | Performs a 'git' 'fetch'.
fetchIO :: Maybe FilePath -> IO ()
fetchIO path = do
  putStrLn "Fetching..."
  sh_ "git fetch --prune" path

-- | Gets all local branches.
getBranchesIO :: Maybe FilePath -> IO LocalBranches
getBranchesIO path = do
  putStrLn "Parsing branches..."
  branchesText <- sh "git branch" path
  case textToLocalBranches branchesText of
    Just x -> pure x
    Nothing -> Ex.die "Error getting local branches"

-- | Updates branch 'Name' according to the 'MergeType'.
updateBranchIO :: MergeType -> Name -> Maybe FilePath -> IO UpdateResult
updateBranchIO merge nm@(Name name) path = do
  putStrLn $ "Checking out " <> T.unpack name
  sh_ ("git checkout \"" <> name <> "\"") path
  putStrLn $ "Updating " <> T.unpack name
  res <- trySh (mergeTypeToCmd merge) path
  case res of
    Right o
      | branchUpToDate o -> pure $ NoChange nm
      | otherwise -> pure $ Success nm
    Left ex -> do
      putStrLn $ "Error updating " <> T.unpack name <> ": " <> show ex
      pure $ Failure nm

-- | Pushes branch 'Name'.
pushBranchIO :: Maybe FilePath -> Name -> IO UpdateResult
pushBranchIO path nm@(Name name) = do
  putStrLn $ "Pushing " <> T.unpack name
  -- "git push" returns the "up to date..." string as stderr for
  -- some reason...
  res <- tryShCaptureErr_ ("git push " <> name) path
  case res of
    Right o
      | remoteUpToDate o -> pure $ NoChange nm
      | otherwise -> pure $ Success nm
    Left ex -> do
      putStrLn $ "Error updating " <> T.unpack name <> ": " <> show ex
      pure $ Failure nm

-- | Prints a summary of all update results.
summarizeIO :: [UpdateResult] -> IO ()
summarizeIO = putStrLn . displayResults

-- | Checks out the 'CurrentBranch' on 'Maybe' 'FilePath'.
checkoutCurrentIO :: CurrentBranch -> Maybe FilePath -> IO ()
checkoutCurrentIO (Name name) = sh_ ("git checkout \"" <> name <> "\"")

mergeTypeToCmd :: MergeType -> T.Text
mergeTypeToCmd Upstream = "git merge @{u} --ff-only"
mergeTypeToCmd Master = "git merge origin/master --ff-only"
mergeTypeToCmd (Other (Name up)) = "git merge \"" <> up <> "\" --ff-only"
