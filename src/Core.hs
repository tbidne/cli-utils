{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Core
( AppT(..)
, runUtils
) where

import Control.Monad.Reader

import Env
import GitTypes
import GitUtils
import Results

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

instance GitUtils m => GitUtils (AppT m) where
  type UtilsResult (AppT m) = UtilsResult m

  grepBranches :: Env -> AppT m (UtilsResult m [Name])
  grepBranches = lift . grepBranches

  isMerged :: Env -> Name -> AppT m (UtilsResult m Bool)
  isMerged env = lift . isMerged env

  logAuthDate :: Env -> Name -> AppT m (UtilsResult m NameLog)
  logAuthDate env = lift . logAuthDate env

  collectResults :: Env -> AppT m Results
  collectResults = lift . collectResults

  display :: Results -> AppT m ()
  display = lift . display

runUtils :: GitUtils m => AppT m Results
runUtils = do
  e <- ask
  collectResults e