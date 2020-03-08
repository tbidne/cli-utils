{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Core.MonadGit
  ( MonadGit (..),
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
import Types.GitTypes
import Types.ResultsWithErrs

class Monad m => MonadGit m where
  type GitType m a :: K.Type
  type ResultType m :: K.Type

  grepBranches :: m [Name]
  getStaleLogs :: [Name] -> m (Filtered (GitType m NameAuthDay))
  toBranches :: Filtered (GitType m NameAuthDay) -> m [GitType m AnyBranch]
  collectResults :: [GitType m AnyBranch] -> m (ResultType m)
  display :: ResultType m -> m ()

instance R.MonadIO m => MonadGit (AppT m) where
  type GitType (AppT m) a = ErrOr a
  type ResultType (AppT m) = ResultsWithErrs

  grepBranches :: (AppT m) [Name]
  grepBranches = do
    p <- R.asks path
    searchStr <- R.asks grepStr
    let branchFn = case searchStr of
          Nothing -> not . badBranch
          Just s ->
            \t -> (not . badBranch) t && T.toCaseFold s `T.isInfixOf` T.toCaseFold t
        toNames' = fmap (Name . T.strip) . filter branchFn . T.lines
    res <- R.liftIO $ sh "git branch -r" p
    R.liftIO $ logIfErr $ return $ toNames' res

  getStaleLogs :: [Name] -> (AppT m) (Filtered (ErrOr NameAuthDay))
  getStaleLogs ns = do
    day <- R.asks today
    lim <- R.asks limit
    p <- R.asks path
    let staleFilter' = mkFiltered $ staleNonErr lim day
    logs <- R.liftIO $ Par.parallelE (fmap (nameToLog p) ns)
    R.liftIO $ return $ (staleFilter' . fmap exceptToErr) logs

  toBranches :: Filtered (ErrOr NameAuthDay) -> (AppT m) [ErrOr AnyBranch]
  toBranches ns = do
    p <- R.asks path
    branches <- R.liftIO $ Par.parallelE (fmap (errTupleToBranch p) (unFiltered ns))
    R.liftIO $ return $ fmap exceptToErr branches

  collectResults :: [ErrOr AnyBranch] -> (AppT m) ResultsWithErrs
  collectResults = return . toResultsWithErrs

  display :: ResultsWithErrs -> (AppT m) ()
  display = R.liftIO . print

runGitUtils :: MonadGit m => m ()
runGitUtils = do
  branchNames <- grepBranches
  staleLogs <- getStaleLogs branchNames
  staleBranches <- toBranches staleLogs
  res <- collectResults staleBranches
  display res
