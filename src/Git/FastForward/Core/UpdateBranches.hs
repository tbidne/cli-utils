{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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

class Monad m => UpdateBranches m where
  fetch :: m ()
  getBranches :: m LocalBranches
  updateBranch :: Name -> m UpdateResult
  summarize :: [UpdateResult] -> m ()
  checkoutCurrent :: CurrentBranch -> m ()

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

  summarize :: [UpdateResult] -> AppT Env m ()
  summarize = R.liftIO . summarizeIO

  checkoutCurrent :: CurrentBranch -> AppT Env m ()
  checkoutCurrent curr = do
    Env {path} <- R.ask
    R.liftIO $ checkoutCurrentIO curr path

runUpdateBranches :: UpdateBranches m => m ()
runUpdateBranches = do
  fetch
  LocalBranches {current, branches} <- getBranches
  res <- traverse updateBranch branches
  summarize res
  checkoutCurrent current