{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.IO
  ( checkoutCurrentIO,
    fetchIO,
    getBranchesIO,
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

fetchIO :: Maybe FilePath -> IO ()
fetchIO path = do
  putStrLn "Fetching..."
  sh_ "git fetch --prune" path

getBranchesIO :: Maybe FilePath -> IO LocalBranches
getBranchesIO path = do
  putStrLn "Parsing branches..."
  branchesText <- sh "git branch" path
  case textToLocalBranches branchesText of
    Just x -> pure x
    Nothing -> Ex.die "Error getting local branches"

updateBranchIO :: MergeType -> Name -> Maybe FilePath -> IO UpdateResult
updateBranchIO merge nm@(Name name) path = do
  putStrLn $ "Checking out " <> T.unpack name
  sh_ ("git checkout " <> name) path
  putStrLn $ "Updating " <> T.unpack name
  res <- trySh (mergeTypeToCmd merge) path
  case res of
    Right o
      | alreadyUpdated o -> pure $ NoChange nm
      | otherwise -> pure $ Success nm
    Left ex -> do
      putStrLn $ "Error updating " <> T.unpack name <> ": " <> show ex
      pure $ Failure nm

summarizeIO :: [UpdateResult] -> IO ()
summarizeIO = putStrLn . displayResults

checkoutCurrentIO :: CurrentBranch -> Maybe FilePath -> IO ()
checkoutCurrentIO (Name name) = sh_ ("git checkout " <> name)

mergeTypeToCmd :: MergeType -> T.Text
mergeTypeToCmd Upstream = "git merge @{u} --ff-only"
mergeTypeToCmd Master = "git merge origin/master --ff-only"
mergeTypeToCmd (Other (Name up)) = "git merge " <> up <> " --ff-only"
