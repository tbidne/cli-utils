{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module App
( AppT(..)
) where

import Control.Monad.Reader

import GitUtils.Core
import Types.Branch
import Types.Env
import Types.GitTypes

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

instance GitUtils m => GitUtils (AppT m) where
  type UtilsType (AppT m) = UtilsType m
  type UtilsResult (AppT m) = UtilsResult m

  grepBranches :: Env -> AppT m [Name]
  grepBranches = lift . grepBranches

  collectResults :: [UtilsType m AnyBranch] -> AppT m (UtilsResult m)
  collectResults = lift . collectResults

  display :: UtilsResult m -> AppT m ()
  display = lift . display

  parseStaleBranches :: Env -> [Name] -> AppT m [UtilsType m AnyBranch]
  parseStaleBranches env = lift . parseStaleBranches env