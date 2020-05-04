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
import Common.MonadLogger
import Common.RefinedUtils
import qualified Control.Concurrent.ParallelIO.Global as Par
import qualified Control.Monad.Reader as R
import qualified Data.Kind as K
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
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
  branchNamesByGrep ::
    Maybe FilePath ->
    BranchType ->
    Maybe T.Text ->
    m [Handler m Name]

  -- | Maps [`Name`] to [`NameAuthDay`], filtering out non-stale branches.
  getStaleLogs ::
    Maybe FilePath ->
    RNonNegative Int ->
    Cal.Day ->
    [Handler m Name] ->
    m (Filtered (Handler m NameAuthDay))

  -- | Maps [`NameAuthDay`] to [`AnyBranch`].
  toBranches ::
    Maybe FilePath ->
    T.Text ->
    Filtered (Handler m NameAuthDay) ->
    m [Handler m AnyBranch]

  -- | Collects [`AnyBranch`] into `FinalResults`.
  collectResults :: [Handler m AnyBranch] -> m (FinalResults m)

  -- | Displays results.
  display :: T.Text -> FinalResults m -> m ()

-- | `MonadFindBranches` instance `IO`. This means we can encounter
-- exceptions, but
--
--   * We do not want a single exception trying to parse one branch kill
--     the entire program.
--   * We do not want to ignore problems entirely.
--
-- We collect the results in `ResultsWithErrs`, which contains a list of errors
-- and two maps, one for merged branches and another for unmerged branches.
-- We opt to define `Handler` as (`ErrOr` a) -- an alias for
-- (`Either` `Err` a) -- as we do not want a single error to crash the app.
-- instance (MonadLogger m, R.MonadIO m) => MonadFindBranches (AppT Env m) where
instance MonadFindBranches IO where
  type Handler IO a = ErrOr a

  type FinalResults IO = ResultsWithErrs

  branchNamesByGrep ::
    Maybe FilePath ->
    BranchType ->
    Maybe T.Text ->
    IO [ErrOr Name]
  branchNamesByGrep path branchType grepStr = do
    let branchFn = case grepStr of
          Nothing -> not . badBranch
          Just s ->
            \t ->
              (not . badBranch) t
                && T.toCaseFold s `T.isInfixOf` T.toCaseFold t
        toNames' = fmap textToName . filter branchFn . T.lines
        cmd = "git branch " <> T.pack (branchTypeToArg branchType)
    res <- sh cmd path
    logIfErr $ pure $ toNames' res

  getStaleLogs ::
    Maybe FilePath ->
    RNonNegative Int ->
    Cal.Day ->
    [ErrOr Name] ->
    IO (Filtered (ErrOr NameAuthDay))
  getStaleLogs path limit today ns = do
    let staleFilter' = mkFiltered $ staleNonErr limit today
    logs <- Par.parallelE (fmap (nameToLog path) ns)
    pure $ (staleFilter' . fmap exceptToErr) logs

  toBranches ::
    Maybe FilePath ->
    T.Text ->
    Filtered (ErrOr NameAuthDay) ->
    IO [ErrOr AnyBranch]
  toBranches path master ns = do
    branches <-
      Par.parallelE
        (fmap (errTupleToBranch path master) (unFiltered ns))
    pure $ fmap exceptToErr branches

  collectResults :: [ErrOr AnyBranch] -> IO ResultsWithErrs
  collectResults = pure . toResultsWithErrs

  display :: T.Text -> ResultsWithErrs -> IO ()
  display remoteName = logResultsWithErrs . toResultsErrDisp remoteName

instance MonadFindBranches m => MonadFindBranches (AppT Env m) where
  type Handler (AppT Env m) a = Handler m a
  type FinalResults (AppT Env m) = FinalResults m

  branchNamesByGrep path branchType = R.lift . branchNamesByGrep path branchType
  getStaleLogs path nat day = R.lift . getStaleLogs path nat day
  toBranches path master = R.lift . toBranches path master
  collectResults = R.lift . collectResults
  display remoteName = R.lift . display remoteName

-- | High level logic of `MonadFindBranches` usage. This function is the
-- entrypoint for any `MonadFindBranches` instance.
runFindBranches :: (R.MonadReader Env m, MonadFindBranches m) => m ()
runFindBranches = do
  Env {path, branchType, grepStr, limit, remoteName, master, today} <- R.ask
  branchNames <- branchNamesByGrep path branchType grepStr
  staleLogs <- getStaleLogs path limit today branchNames
  staleBranches <- toBranches path master staleLogs
  res <- collectResults staleBranches
  display remoteName res

logResultsWithErrs :: MonadLogger m => ResultsWithErrsDisp -> m ()
logResultsWithErrs (ResultsWithErrsDisp (errDisp, resultsDisp)) = do
  let (ErrDisp (errs, numErrs)) = errDisp
  logWarn $
    "\n\nERRORS: "
      <> T.pack (show numErrs)
      <> "\n------\n"
      <> errs
      <> "\n"
  let (ResultsDisp (MergedDisp (ms, numMs), UnMergedDisp (unms, numUnMs))) = resultsDisp
  logInfoSuccess $
    "\n\nMERGED: "
      <> T.pack (show numMs)
      <> "\n------\n"
      <> ms
  logInfoCyan $
    "\n\nUNMERGED: "
      <> T.pack (show numUnMs)
      <> "\n--------\n"
      <> unms
