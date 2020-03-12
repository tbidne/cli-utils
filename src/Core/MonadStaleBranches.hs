{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Core.MonadStaleBranches
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- The MonadStaleBranches class.
module Core.MonadStaleBranches
  ( MonadStaleBranches (..),
    runGitUtils,
  )
where

import App
import qualified Control.Concurrent.ParallelIO.Global as Par
import qualified Control.Monad.Reader as R
import Core.IO
import Core.Internal
import qualified Data.Kind as K
import qualified Data.Text as T
import Types.Branch
import Types.Env
import Types.Error
import Types.Filtered
import Types.GitTypes
import Types.ResultsWithErrs

-- | The 'MonadStaleBranches' class is used to describe interacting with a
-- git filesystem.
class Monad m => MonadStaleBranches m where
  -- | Adds custom handling to returned data (e.g. for error handling).
  type Handler m a :: K.Type

  -- | The type returned by `collectResults`.
  type FinalResults m :: K.Type

  -- | Returns a [`Name`] representing git branches.
  branchNamesByGrep :: m [Handler m Name]

  -- | Maps [`Name`] to [`NameAuthDay`], filtering out non-stale branches.
  getStaleLogs :: [Handler m Name] -> m (Filtered (Handler m NameAuthDay))

  -- | Maps [`NameAuthDay`] to [`AnyBranch`].
  toBranches :: Filtered (Handler m NameAuthDay) -> m [Handler m AnyBranch]

  -- | Collects [`AnyBranch`] into `FinalResults`.
  collectResults :: [Handler m AnyBranch] -> m (FinalResults m)

  -- | Displays results.
  display :: FinalResults m -> m ()

-- | `MonadStaleBranches` instance for (`AppT` m) over `R.MonadIO`. This means we can
-- encounter exceptions, but
--
--   * We do not want a single exception trying to parse one branch kill
--     the entire program.
--   * We do not want to ignore problems entirely.
--
-- We opt to define `Handler` as (`ErrOr` a), which is an alias for (`Either` `Err` a).
-- We collect the results in `ResultsWithErrs`, which contains a list of errors
-- and two maps, one for merged branches and another for unmerged branches.
instance R.MonadIO m => MonadStaleBranches (AppT m) where
  type Handler (AppT m) a = ErrOr a

  type FinalResults (AppT m) = ResultsWithErrs

  branchNamesByGrep :: (AppT m) [ErrOr Name]
  branchNamesByGrep = do
    Env {branchType, grepStr, path} <- R.ask
    let branchFn = case grepStr of
          Nothing -> not . badBranch
          Just s ->
            \t -> (not . badBranch) t && T.toCaseFold s `T.isInfixOf` T.toCaseFold t
        toNames' = fmap textToName . filter branchFn . T.lines
        cmd = "git branch " <> T.pack (branchTypeToArg branchType)
    res <- R.liftIO $ sh cmd path
    R.liftIO $ logIfErr $ pure $ toNames' res

  getStaleLogs :: [ErrOr Name] -> (AppT m) (Filtered (ErrOr NameAuthDay))
  getStaleLogs ns = do
    Env {limit, path, today} <- R.ask
    let staleFilter' = mkFiltered $ staleNonErr limit today
    logs <- R.liftIO $ Par.parallelE (fmap (nameToLog path) ns)
    R.liftIO $ pure $ (staleFilter' . fmap exceptToErr) logs

  toBranches :: Filtered (ErrOr NameAuthDay) -> (AppT m) [ErrOr AnyBranch]
  toBranches ns = do
    Env {path, master} <- R.ask
    branches <- R.liftIO $ Par.parallelE (fmap (errTupleToBranch path master) (unFiltered ns))
    R.liftIO $ pure $ fmap exceptToErr branches

  collectResults :: [ErrOr AnyBranch] -> (AppT m) ResultsWithErrs
  collectResults = pure . toResultsWithErrs

  display :: ResultsWithErrs -> (AppT m) ()
  display res = do
    Env {remoteName} <- R.ask
    R.liftIO $ putStrLn $ T.unpack $ displayResultsWithErrs remoteName res

-- | High level logic of `MonadStaleBranches` usage. This function is the
-- entrypoint for any `MonadStaleBranches` instance.
runGitUtils :: MonadStaleBranches m => m ()
runGitUtils = do
  branchNames <- branchNamesByGrep
  staleLogs <- getStaleLogs branchNames
  staleBranches <- toBranches staleLogs
  res <- collectResults staleBranches
  display res
