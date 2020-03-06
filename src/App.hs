{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilies               #-}

module App
  ( AppT(..)
  )
where

import           Control.Monad.Reader

import           Core.MonadGit
import           Types.Branch
import           Types.Env
import           Types.GitTypes

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

newtype AppType a = AppType { unAppType :: a }
newtype AppResult a = AppResult { unAppResult :: a }
instance MonadGit m => MonadGit (AppT m) where
  type GitType (AppT m) a = AppType (GitType m a)
  type ResultType (AppT m) = AppResult (ResultType m)

  grepBranches :: Env -> AppT m [Name]
  grepBranches = lift . grepBranches

  getStaleLogs
    :: Env -> [Name] -> AppT m (Filtered (AppType (GitType m NameAuthDay)))
  getStaleLogs env = lift . (fmap . fmap) AppType . getStaleLogs env

  toBranches
    :: Env
    -> Filtered (AppType (GitType m NameAuthDay))
    -> AppT m [AppType (GitType m AnyBranch)]
  toBranches env =
    lift . (fmap . fmap) AppType . toBranches env . fmap unAppType

  collectResults
    :: [AppType (GitType m AnyBranch)] -> AppT m (AppResult (ResultType m))
  collectResults = lift . fmap AppResult . collectResults . fmap unAppType

  display :: AppResult (ResultType m) -> AppT m ()
  display = lift . display . unAppResult
