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
import Common.Logging
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as R
import qualified Data.Text as T
import Git.FastForward.Core.Internal
import Git.FastForward.Types.Env
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes
import qualified System.Exit as Ex

-- | The 'MonadUpdateBranches' class is used to describe updating branches
-- on a git filesystem.
class Monad m => MonadUpdateBranches m where
  -- | Performs 'fetch'.
  fetch :: m ()

  -- | Retrieves all local branches.
  getBranches :: m LocalBranches

  -- | Updates a branch by 'Name', returns the result.
  updateBranch :: Name -> m UpdateResult

  -- | Pushes branches, returns the results.
  pushBranches :: m [UpdateResult]

  -- | Checks out the passed 'CurrentBranch'.
  checkoutCurrent :: CurrentBranch -> m ()

-- | `MonadUpdateBranches` instance for (`AppT` m) over `R.MonadIO`. In general, we
-- do not care about error handling /except/ during 'updateBranch'. This is
-- because a failure during 'updateBranch' is relatively common as we're
-- being conservative by merging with "--ff-only", and we'd rather log it
-- and continue trying to update other branches.
instance R.MonadIO m => MonadUpdateBranches (AppT Env m) where
  fetch :: AppT Env m ()
  fetch = do
    Env {path} <- R.ask
    logInfo "Fetching..."
    res <- R.liftIO $ tryShExitCode "git fetch --prune" path
    case res of
      Left t -> do
        logError t
        R.liftIO Ex.exitFailure
      Right _ -> pure ()

  getBranches :: AppT Env m LocalBranches
  getBranches = do
    Env {path} <- R.ask
    logInfo "Parsing branches..."
    res <- R.liftIO $ tryShExitCode "git branch" path
    case res >>= textToLocalBranches of
      Left t -> do
        logError t
        R.liftIO Ex.exitFailure
      Right x -> pure x

  updateBranch :: Name -> AppT Env m UpdateResult
  updateBranch nm@(Name name) = do
    Env {mergeType, path} <- R.ask
    let checkout = tryShExitCode ("git checkout \"" <> name <> "\"") path
        update = tryShExitCode (mergeTypeToCmd mergeType) path
    logInfo $ "Updating " <> name
    res <- R.liftIO $ failFast checkout update
    case res of
      Left t -> do
        logWarn t
        pure $ Failure nm
      Right o
        | branchUpToDate o -> pure $ NoChange nm
        | otherwise -> pure $ Success nm

  pushBranches :: AppT Env m [UpdateResult]
  pushBranches = do
    Env {path, push} <- R.ask
    traverse (pushBranch path) push

  checkoutCurrent :: CurrentBranch -> AppT Env m ()
  checkoutCurrent (Name name) = do
    Env {path} <- R.ask
    R.liftIO $ sh_ ("git checkout \"" <> name <> "\"") path

pushBranch :: R.MonadIO m => Maybe FilePath -> Name -> AppT Env m UpdateResult
pushBranch path nm@(Name name) = do
  logInfo $ "Pushing " <> name
  -- "git push" returns the "up to date..." string as stderr for
  -- some reason...
  res <- R.liftIO $ tryShAndReturnStdErr ("git push " <> name) path
  case res of
    Left t -> do
      logWarn t
      pure $ Failure nm
    Right o
      | remoteUpToDate o -> pure $ NoChange nm
      | otherwise -> pure $ Success nm

-- | High level logic of `MonadUpdateBranches` usage. This function is the
-- entrypoint for any `MonadUpdateBranches` instance.
runUpdateBranches :: (ML.MonadLogger m, MonadUpdateBranches m) => m ()
runUpdateBranches = do
  fetch
  LocalBranches {current, branches} <- getBranches
  updated <- traverse updateBranch branches
  logInfo ""
  logInfoBlue "UPDATE SUMMARY"
  logInfoBlue "--------------"
  displaySplits $ splitResults updated
  pushed <- pushBranches
  logInfo ""
  logInfoBlue "PUSH SUMMARY"
  logInfoBlue "------------"
  displaySplits $ splitResults pushed
  checkoutCurrent current

displaySplits :: ML.MonadLogger m => SplitResults -> m ()
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
