{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Core.MonadGit
  ( MonadGit(..)
  , runWithReader
  )
where

import qualified Control.Concurrent.ParallelIO.Global as Par
import qualified Control.Monad.Reader as R
import qualified Data.Kind as K
import qualified Data.Text as T

import           Core.IO
import           Core.Internal
import           Types.Branch
import           Types.Env
import           Types.Error
import           Types.GitTypes
import           Types.ResultsWithErrs

class Monad m => MonadGit m where
  type GitType (m :: K.Type -> K.Type) (a :: K.Type) = (r :: K.Type) | r -> m a

  type ResultType (m :: K.Type -> K.Type) = (r :: K.Type) | r -> m

  grepBranches :: Env -> m [Name]
  getStaleLogs :: Env -> [Name] -> m (Filtered (GitType m NameAuthDay))
  toBranches :: Env -> Filtered (GitType m NameAuthDay) -> m [GitType m AnyBranch]
  collectResults :: [GitType m AnyBranch] -> m (ResultType m)
  display :: ResultType m -> m ()

instance MonadGit IO where
  type GitType IO a = ErrOr a
  type ResultType IO = ResultsWithErrs

  grepBranches :: Env -> IO [Name]
  grepBranches Env {..} = do
    res <- sh "git branch -r" path
    logIfErr $ return $ toNames res
   where
    f' = case grepStr of
      Nothing -> not . badBranch
      Just s ->
        \t -> (not . badBranch) t && T.toCaseFold s `T.isInfixOf` T.toCaseFold t
    toNames = fmap (Name . T.strip) . filter f' . T.lines

  getStaleLogs :: Env -> [Name] -> IO (Filtered (ErrOr NameAuthDay))
  getStaleLogs env@Env {..} ns = do
    logs <- Par.parallelE (fmap (nameToLog env) ns)
    let filteredLogs = (filter' . fmap exceptToErr) logs
    return filteredLogs
    where filter' = mkFiltered $ staleNonErr limit today

  toBranches :: Env -> Filtered (ErrOr NameAuthDay) -> IO [ErrOr AnyBranch]
  toBranches env ns = do
    branches <- Par.parallelE (fmap (errTupleToBranch env) (unFiltered ns))
    return $ fmap exceptToErr branches

  collectResults :: [ErrOr AnyBranch] -> IO ResultsWithErrs
  collectResults = return . toResultsWithErrs

  display :: ResultsWithErrs -> IO ()
  display = print

runWithReader :: (MonadGit m, R.MonadReader Env m) => m ()
runWithReader = do
  env           <- R.ask
  branchNames   <- grepBranches env
  staleLogs     <- getStaleLogs env branchNames
  staleBranches <- toBranches env staleLogs
  res           <- collectResults staleBranches
  display res
