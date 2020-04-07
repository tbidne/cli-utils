{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Git.Stale.Core.MonadFindBranches
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- The MonadFindBranches class.
module Git.Stale.Core.MonadFindBranches
  ( MonadFindBranches (..),
    runFindBranches,
  )
where

import App
import Common.Logging
import qualified Control.Concurrent.ParallelIO.Global as Par
import qualified Control.Monad.Logger as L
import qualified Control.Monad.Reader as R
import qualified Data.Kind as K
import qualified Data.Text as T
import Git.Stale.Core.IO
import Git.Stale.Core.Internal
import Git.Stale.Types.Branch
import Git.Stale.Types.Env
import Git.Stale.Types.Error
import Git.Stale.Types.Filtered
import Git.Stale.Types.Results
import Git.Stale.Types.ResultsWithErrs
import Git.Types.GitTypes

-- | The 'MonadFindBranches' class is used to describe various git
-- actions for finding stale branches.
class Monad m => MonadFindBranches m where
  -- | Adds custom handling to returned data (e.g. for error handling).
  type Handler (m :: K.Type -> K.Type) (a :: K.Type)

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

-- | `MonadFindBranches` instance for (`AppT` m) over `R.MonadIO`. This means we can
-- encounter exceptions, but
--
--   * We do not want a single exception trying to parse one branch kill
--     the entire program.
--   * We do not want to ignore problems entirely.
--
-- We collect the results in `ResultsWithErrs`, which contains a list of errors
-- and two maps, one for merged branches and another for unmerged branches.
-- We opt to define `Handler` as (`ErrOr` a) -- an alias for
-- (`Either` `Err` a) -- as we do not want a single error to crash the app.
instance R.MonadIO m => MonadFindBranches (AppT Env m) where
  type Handler (AppT Env m) a = ErrOr a

  type FinalResults (AppT Env m) = ResultsWithErrs

  branchNamesByGrep :: (AppT Env m) [ErrOr Name]
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

  getStaleLogs :: [ErrOr Name] -> (AppT Env m) (Filtered (ErrOr NameAuthDay))
  getStaleLogs ns = do
    Env {limit, path, today} <- R.ask
    let staleFilter' = mkFiltered $ staleNonErr limit today
    logs <- R.liftIO $ Par.parallelE (fmap (nameToLog path) ns)
    R.liftIO $ pure $ (staleFilter' . fmap exceptToErr) logs

  toBranches :: Filtered (ErrOr NameAuthDay) -> (AppT Env m) [ErrOr AnyBranch]
  toBranches ns = do
    Env {path, master} <- R.ask
    branches <- R.liftIO $ Par.parallelE (fmap (errTupleToBranch path master) (unFiltered ns))
    R.liftIO $ pure $ fmap exceptToErr branches

  collectResults :: [ErrOr AnyBranch] -> (AppT Env m) ResultsWithErrs
  collectResults = pure . toResultsWithErrs

  display :: ResultsWithErrs -> (AppT Env m) ()
  display res = do
    Env {remoteName} <- R.ask
    logResultsWithErrs $ toResultsErrDisp remoteName res

-- | High level logic of `MonadFindBranches` usage. This function is the
-- entrypoint for any `MonadFindBranches` instance.
runFindBranches :: MonadFindBranches m => m ()
runFindBranches = do
  branchNames <- branchNamesByGrep
  staleLogs <- getStaleLogs branchNames
  staleBranches <- toBranches staleLogs
  res <- collectResults staleBranches
  display res

logResultsWithErrs :: L.MonadLogger m => ResultsWithErrsDisp -> m ()
logResultsWithErrs (ResultsWithErrsDisp (errDisp, resultsDisp)) = do
  let (ErrDisp (errs, numErrs)) = errDisp
  logWarn $ "ERRORS: " <> T.pack (show numErrs)
  logWarn "------"
  logWarn $ errs <> "\n"
  let (ResultsDisp (MergedDisp (ms, numMs), UnMergedDisp (unms, numUnMs))) = resultsDisp
  logInfoSuccess $ "MERGED: " <> T.pack (show numMs)
  logInfoSuccess "------"
  logInfoSuccess ms
  logInfoCyan $ "UNMERGED: " <> T.pack (show numUnMs)
  logInfoCyan "--------"
  logInfoCyan unms
