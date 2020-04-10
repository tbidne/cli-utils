{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.FastForward.Core.MonadUpdateBranches
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- The MonadUpdateBranches class.
module Git.FastForward.Core.MonadUpdateBranches
  ( MonadUpdateBranches (..),
    runUpdateBranches,
  )
where

import App
import Common.IO
import Common.MonadLogger
import qualified Control.Monad.Reader as R
import qualified Data.Text as T
import Git.FastForward.Core.Internal
import Git.FastForward.Types.Env
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.MergeType
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes
import qualified System.Exit as Ex

-- | The 'MonadUpdateBranches' class is used to describe updating branches
-- on a git filesystem.
class Monad m => MonadUpdateBranches m where
  -- | Performs 'fetch'.
  fetch :: Maybe FilePath -> m ()

  -- | Retrieves all local branches.
  getBranches :: Maybe FilePath -> m LocalBranches

  -- | Updates a branch by 'Name', returns the result.
  updateBranch :: Maybe FilePath -> MergeType -> Name -> m UpdateResult

  -- | Pushes branches, returns the results.
  pushBranches :: Maybe FilePath -> [Name] -> m [UpdateResult]

  -- | Checks out the passed 'CurrentBranch'.
  checkoutCurrent :: Maybe FilePath -> CurrentBranch -> m ()

-- | `MonadUpdateBranches` instance for `IO`. In general, we do not care
-- about error handling /except/ during 'updateBranch'. This is
-- because a failure during 'updateBranch' is relatively common as we're
-- being conservative by merging with "--ff-only", and we'd rather log it
-- and continue trying to update other branches.
instance MonadUpdateBranches IO where
  fetch :: Maybe FilePath -> IO ()
  fetch path = do
    logInfo "Fetching..."
    res <- tryShExitCode "git fetch --prune" path
    case res of
      Left t -> do
        logError t
        Ex.exitFailure
      Right _ -> pure ()

  getBranches :: Maybe FilePath -> IO LocalBranches
  getBranches path = do
    logInfo "Parsing branches..."
    res <- tryShExitCode "git branch" path
    case res >>= textToLocalBranches of
      Left t -> do
        logError t
        Ex.exitFailure
      Right x -> pure x

  updateBranch :: Maybe FilePath -> MergeType -> Name -> IO UpdateResult
  updateBranch path mergeType nm@(Name name) = do
    let checkout = tryShExitCode ("git checkout \"" <> name <> "\"") path
        update = tryShExitCode (mergeTypeToCmd mergeType) path
    logInfo $ "Updating " <> name
    res <- failFast checkout update
    case res of
      Left t -> do
        logWarn t
        pure $ Failure nm
      Right o
        | branchUpToDate o -> pure $ NoChange nm
        | otherwise -> pure $ Success nm

  pushBranches :: Maybe FilePath -> [Name] -> IO [UpdateResult]
  pushBranches path branches = do
    traverse (pushBranch path) branches

  checkoutCurrent :: Maybe FilePath -> CurrentBranch -> IO ()
  checkoutCurrent path (Name name) = do
    sh_ ("git checkout \"" <> name <> "\"") path

pushBranch :: Maybe FilePath -> Name -> IO UpdateResult
pushBranch path nm@(Name name) = do
  logInfo $ "Pushing " <> name
  -- "git push" returns the "up to date..." string as stderr for
  -- some reason...
  res <- tryShAndReturnStdErr ("git push " <> name) path
  case res of
    Left t -> do
      logWarn t
      pure $ Failure nm
    Right o
      | remoteUpToDate o -> pure $ NoChange nm
      | otherwise -> pure $ Success nm

instance MonadUpdateBranches m => MonadUpdateBranches (AppT Env m) where
  fetch = R.lift . fetch
  getBranches = R.lift . getBranches
  updateBranch path mergeType = R.lift . updateBranch path mergeType
  pushBranches path = R.lift . pushBranches path
  checkoutCurrent path = R.lift . checkoutCurrent path

-- | High level logic of `MonadUpdateBranches` usage. This function is the
-- entrypoint for any `MonadUpdateBranches` instance.
runUpdateBranches :: (R.MonadReader Env m, MonadLogger m, MonadUpdateBranches m) => m ()
runUpdateBranches = do
  Env {path, mergeType, push} <- R.ask
  fetch path
  LocalBranches {current, branches} <- getBranches path
  updated <- traverse (updateBranch path mergeType) branches
  logUpdate updated
  pushed <- pushBranches path push
  logPush pushed
  checkoutCurrent path current

logUpdate :: MonadLogger m => [UpdateResult] -> m ()
logUpdate updated = do
  logInfo ""
  logInfoBlue "UPDATE SUMMARY"
  logInfoBlue "--------------"
  displaySplits $ splitResults updated

logPush :: MonadLogger m => [UpdateResult] -> m ()
logPush updated = do
  logInfo ""
  logInfoBlue "PUSH SUMMARY"
  logInfoBlue "------------"
  displaySplits $ splitResults updated

displaySplits :: MonadLogger m => SplitResults -> m ()
displaySplits SplitResults {successes, noChanges, failures} = do
  logInfoSuccess $ "Successes: " <> T.pack (show successes)
  logInfo $ "No Change: " <> T.pack (show noChanges)
  logWarn $ "Failures: " <> T.pack (show failures) <> "\n"

failFast :: IO (Either e a) -> IO (Either e a) -> IO (Either e a)
failFast io1 io2 = do
  res <- io1
  case res of
    Left e -> pure $ Left e
    Right _ -> io2
