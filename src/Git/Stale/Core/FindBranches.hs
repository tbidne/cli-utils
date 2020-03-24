{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Git.Stale.Core.FindBranches
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- The FindBranches class.
module Git.Stale.Core.FindBranches
  ( FindBranches (..),
    runFindBranches,
  )
where

import App
import qualified Control.Concurrent.ParallelIO.Global as Par
import qualified Control.Monad.Reader as R
import Git.Stale.Core.IO
import Git.Stale.Core.Internal
import qualified Data.Kind as K
import qualified Data.Text as T
import Git.Stale.Types.Branch
import Git.Stale.Types.Env
import Git.Stale.Types.Error
import Git.Stale.Types.Filtered
import Git.Types.GitTypes
import Git.Stale.Types.ResultsWithErrs
import Git.Types.Handler

-- | The 'FindBranches' class is used to describe interacting with a
-- git filesystem.
class Monad m => FindBranches m where
  -- | Adds custom handling to returned data (e.g. for error handling).
  --type Handler m a :: K.Type

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


type instance Handler (AppT Env m) a = ErrOr a

-- | `FindBranches` instance for (`AppT` m) over `R.MonadIO`. This means we can
-- encounter exceptions, but
--
--   * We do not want a single exception trying to parse one branch kill
--     the entire program.
--   * We do not want to ignore problems entirely.
--
-- We opt to define `Handler` as (`ErrOr` a), which is an alias for (`Either` `Err` a).
-- We collect the results in `ResultsWithErrs`, which contains a list of errors
-- and two maps, one for merged branches and another for unmerged branches.
instance R.MonadIO m => FindBranches (AppT Env m) where
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
    R.liftIO $ putStrLn $ T.unpack $ displayResultsWithErrs remoteName res

-- | High level logic of `FindBranches` usage. This function is the
-- entrypoint for any `FindBranches` instance.
runFindBranches :: FindBranches m => m ()
runFindBranches = do
  branchNames <- branchNamesByGrep
  staleLogs <- getStaleLogs branchNames
  staleBranches <- toBranches staleLogs
  res <- collectResults staleBranches
  display res
