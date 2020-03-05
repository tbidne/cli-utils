{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Core.MonadGit
( MonadGit(..)
, runWithReader
) where

import           Control.Monad.Reader (MonadReader, ask)
import           Control.Concurrent.ParallelIO.Global (parallelE)
import           Data.Kind (Type)
import qualified Data.Text as Txt

import Core.Internal
import Core.IO
import Types.Branch
import Types.Env
import Types.Error
import Types.GitTypes
import Types.ResultsWithErrs

class Monad m => MonadGit m where
  type UtilsType (m :: Type -> Type) (a :: Type)
    = (r :: Type) | r -> m a
  
  type UtilsResult (m :: Type -> Type)
    = (r :: Type) | r -> m

  grepBranches :: Env -> m [Name]
  getStaleLogs :: Env -> [Name] -> m (Filtered (UtilsType m NameAuthDay))
  toBranches :: Env -> Filtered (UtilsType m NameAuthDay) -> m [UtilsType m AnyBranch]
  collectResults :: [UtilsType m AnyBranch] -> m (UtilsResult m)
  display :: (UtilsResult m) -> m ()

instance MonadGit IO where
  type UtilsType IO a = ErrOr a
  type UtilsResult IO = ResultsWithErrs

  grepBranches :: Env -> IO [Name]
  grepBranches Env{..} = do
    res <- sh "git branch -r" path

    logIfErr $ return $ toNames res

    where maybeFilter = case grepStr of
            Nothing -> id
            Just s  -> filter (\t -> (Txt.toCaseFold s) `Txt.isInfixOf` (Txt.toCaseFold t))
          toNames = fmap (Name . Txt.strip) . maybeFilter . Txt.lines

  getStaleLogs :: Env -> [Name] -> IO (Filtered (ErrOr NameAuthDay))
  getStaleLogs env@Env{..} ns = do
    logs <- parallelE (fmap (nameToLog env) ns)

    let filteredLogs = (filter' . fmap exceptToErr) logs

    return filteredLogs
    
    where filter' = mkFiltered $ staleNonErr limit today

  toBranches :: Env -> Filtered (ErrOr NameAuthDay) -> IO [ErrOr AnyBranch]
  toBranches env ns = do
    branches <- parallelE (fmap (errTupleToBranch env) (unFiltered ns))

    return $ fmap exceptToErr branches

  collectResults :: [ErrOr AnyBranch] -> IO ResultsWithErrs
  collectResults = return . toResultsWithErrs

  display :: ResultsWithErrs -> IO ()
  display = print

runWithReader :: (MonadGit m, MonadReader Env m) => m ()
runWithReader = do
  env <- ask
  branchNames <- grepBranches env

  staleLogs <- getStaleLogs env branchNames

  staleBranches <- toBranches env staleLogs

  res <- collectResults staleBranches

  display res