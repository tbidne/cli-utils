{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Core
( AppT(..)
) where

import Control.Monad.Reader

import Branch
import Env
import GitTypes
import GitUtils

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

instance GitUtils m => GitUtils (AppT m) where
  type UtilsType (AppT m) = UtilsType m
  type UtilsResult (AppT m) = UtilsResult m

  grepBranches :: Env -> AppT m [Name]
  grepBranches = lift . grepBranches

  logAuthDate :: Env -> [Name] -> AppT m [UtilsType m BranchLog]
  logAuthDate env = lift . logAuthDate env

  filterFreshBranches :: Env -> [UtilsType m BranchLog] -> AppT m [UtilsType m BranchLog]
  filterFreshBranches env = lift . filterFreshBranches env

  isMerged :: Env -> Name -> AppT m (UtilsType m Bool)
  isMerged env = lift . isMerged env

  toAnyBranch :: Env -> [UtilsType m BranchLog] -> AppT m [UtilsType m AnyBranch]
  toAnyBranch env = lift . toAnyBranch env

  collectResults :: [UtilsType m AnyBranch] -> AppT m (UtilsResult m)
  collectResults = lift . collectResults

  display :: UtilsResult m -> AppT m ()
  display = lift . display