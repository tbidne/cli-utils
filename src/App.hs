{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module App
( AppT(..)
) where

import Control.Monad.Reader

import Core.MonadGit
import Types.Branch
import Types.Env
import Types.GitTypes

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

newtype AppType a = AppType { unAppType :: a }
newtype AppResult a = AppResult { unAppResult :: a }
instance MonadGit m => MonadGit (AppT m) where
  type UtilsType (AppT m) a = AppType (UtilsType m a)
  type UtilsResult (AppT m) = AppResult (UtilsResult m)

  grepBranches :: Env -> AppT m [Name]
  grepBranches = lift . grepBranches

  parseStaleBranches :: Env -> [Name] -> AppT m [AppType (UtilsType m AnyBranch)]
  parseStaleBranches env = lift . (fmap . fmap) AppType . parseStaleBranches env

  collectResults :: [AppType (UtilsType m AnyBranch)] -> AppT m (AppResult (UtilsResult m))
  collectResults = lift . fmap AppResult . collectResults . fmap unAppType

  display :: AppResult (UtilsResult m) -> AppT m ()
  display = lift . display . unAppResult