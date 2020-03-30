{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : Git.FastForward.Core.UpdateBranches
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- The UpdateBranches class.
module Git.FastForward.Core.UpdateBranches
  ( UpdateBranches (..),
    runUpdateBranches,
  )
where

import App
import qualified Control.Monad.Reader as R
import Git.FastForward.Core.IO
import Git.FastForward.Types.Env
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes

-- | The 'UpdateBranches' class is used to describe updating branches
-- on a git filesystem.
class Monad m => UpdateBranches m where
  -- | Performs 'fetch'.
  fetch :: m ()

  -- | Retrieves all local branches.
  getBranches :: m LocalBranches

  -- | Updates a branch by 'Name', returns the result.
  updateBranch :: Name -> m UpdateResult

  -- | Pushes branches, returns the results.
  pushBranches :: m [UpdateResult]

  -- | Summarizes all results.
  summarize :: [UpdateResult] -> m ()

  -- | Checks out the passed 'CurrentBranch'.
  checkoutCurrent :: CurrentBranch -> m ()

-- | `UpdateBranches` instance for (`AppT` m) over `R.MonadIO`. In general, we
-- do not care about error handling /except/ during 'updateBranch'. This is
-- because a failure during 'updateBranch' is relatively common as we're
-- being conservative by merging with "--ff-only", and we'd rather log it
-- and continue trying to update other branches.
instance R.MonadIO m => UpdateBranches (AppT Env m) where
  fetch :: AppT Env m ()
  fetch = do
    Env {path} <- R.ask
    R.liftIO $ fetchIO path

  getBranches :: AppT Env m LocalBranches
  getBranches = do
    Env {path} <- R.ask
    R.liftIO $ getBranchesIO path

  updateBranch :: Name -> AppT Env m UpdateResult
  updateBranch name = do
    Env {mergeType, path} <- R.ask
    R.liftIO $ updateBranchIO mergeType name path

  pushBranches :: AppT Env m [UpdateResult]
  pushBranches = do
    Env {path, push} <- R.ask
    R.liftIO $ traverse (pushBranchIO path) push

  summarize :: [UpdateResult] -> AppT Env m ()
  summarize = R.liftIO . summarizeIO

  checkoutCurrent :: CurrentBranch -> AppT Env m ()
  checkoutCurrent curr = do
    Env {path} <- R.ask
    R.liftIO $ checkoutCurrentIO curr path

-- | High level logic of `UpdateBranches` usage. This function is the
-- entrypoint for any `UpdateBranches` instance.
runUpdateBranches :: UpdateBranches m => m ()
runUpdateBranches = do
  fetch
  LocalBranches {current, branches} <- getBranches
  res <- traverse updateBranch branches
  summarize res
  pushed <- pushBranches
  summarize pushed
  checkoutCurrent current
