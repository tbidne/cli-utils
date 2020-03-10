{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
import Types.GitTypes
import Types.ResultsWithErrs

class Monad m => MonadStaleBranches m where
  type Handler m a :: K.Type
  type FinalResults m :: K.Type

  branchNamesByGrep :: m [Name]
  getStaleLogs :: [Name] -> m (Filtered (Handler m NameAuthDay))
  toBranches :: Filtered (Handler m NameAuthDay) -> m [Handler m AnyBranch]
  collectResults :: [Handler m AnyBranch] -> m (FinalResults m)
  display :: FinalResults m -> m ()

instance R.MonadIO m => MonadStaleBranches (AppT m) where
  type Handler (AppT m) a = ErrOr a
  type FinalResults (AppT m) = ResultsWithErrs

  branchNamesByGrep :: (AppT m) [Name]
  branchNamesByGrep = do
    p <- R.asks path
    searchStr <- R.asks grepStr
    bType <- R.asks branchType
    let branchFn = case searchStr of
          Nothing -> not . badBranch
          Just s ->
            \t -> (not . badBranch) t && T.toCaseFold s `T.isInfixOf` T.toCaseFold t
        toNames' = fmap (Name . T.strip) . filter branchFn . T.lines
        cmd = ("git branch " <> T.pack (branchTypeToArg bType))
    res <- R.liftIO $ sh cmd p
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

runGitUtils :: MonadStaleBranches m => m ()
runGitUtils = do
  branchNames <- branchNamesByGrep
  staleLogs <- getStaleLogs branchNames
  staleBranches <- toBranches staleLogs
  res <- collectResults staleBranches
  display res
