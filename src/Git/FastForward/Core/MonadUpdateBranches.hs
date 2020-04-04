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
    R.liftIO $ sh_ "git fetch --prune" path

  getBranches :: AppT Env m LocalBranches
  getBranches = do
    Env {path} <- R.ask
    logInfo "Parsing branches..."
    branchesText <- R.liftIO $ sh "git branch" path
    case textToLocalBranches branchesText of
      Just x -> pure x
      Nothing -> do
        logError "Error getting local branches"
        R.liftIO $ Ex.die "Fatal error"

  updateBranch :: Name -> AppT Env m UpdateResult
  updateBranch nm@(Name name) = do
    Env {mergeType, path} <- R.ask
    logInfo $ "Checking out " <> name
    R.liftIO $ sh_ ("git checkout \"" <> name <> "\"") path
    logInfo $ "Updating " <> name
    res <- R.liftIO $ trySh (mergeTypeToCmd mergeType) path
    case res of
      Right o
        | branchUpToDate o -> pure $ NoChange nm
        | otherwise -> pure $ Success nm
      Left ex -> do
        logWarn $
          "Error updating "
            <> name
            <> ": "
            <> T.pack (show ex)
        pure $ Failure nm

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
  res <- R.liftIO $ tryShCaptureErr_ ("git push " <> name) path
  case res of
    Right o
      | remoteUpToDate o -> pure $ NoChange nm
      | otherwise -> pure $ Success nm
    Left ex -> do
      logWarn $
        "Error updating "
          <> name
          <> ": "
          <> T.pack (show ex)
      pure $ Failure nm

-- | High level logic of `MonadUpdateBranches` usage. This function is the
-- entrypoint for any `MonadUpdateBranches` instance.
runUpdateBranches :: (ML.MonadLogger m, MonadUpdateBranches m) => m ()
runUpdateBranches = do
  fetch
  LocalBranches {current, branches} <- getBranches
  updated <- traverse updateBranch branches
  logInfoPretty $ prettyUpdate $ displayResults updated
  pushed <- pushBranches
  logInfoPretty $ prettyPush $ displayResults pushed
  checkoutCurrent current

prettyUpdate :: T.Text -> T.Text
prettyUpdate summary =
  "\n\nUPDATE SUMMARY\n"
    <> "--------------\n"
    <> summary
    <> "\n"

prettyPush :: T.Text -> T.Text
prettyPush summary =
  "\n\nPUSH SUMMARY\n"
    <> "-----------\n"
    <> summary
    <> "\n"
