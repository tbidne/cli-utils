{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.MockUpdateBranches where

import Control.Monad.Logger
import Control.Monad.Reader
import Git.FastForward.Core.MonadUpdateBranches
import Git.FastForward.Types.Env
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes
import Control.Monad.Identity

newtype MockUpdateT m a = MockUpdateT {runMockUpdateT :: ReaderT Env (WriterLoggingT m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env)

instance MonadTrans MockUpdateT where
  lift = MockUpdateT . lift . lift

type MockUpdateI = MockUpdateT Identity
type LogLine = (Loc, LogSource, LogLevel, LogStr)

instance MonadUpdateBranches MockUpdateI where
  fetch :: MockUpdateI ()
  fetch = logInfoN "Fetching"

  getBranches :: MockUpdateI LocalBranches
  getBranches = lift $ pure $
      LocalBranches
        (Name "current")
        [ Name "success1",
          Name "success2",
          Name "noChange1",
          Name "noChange2",
          Name "failure1",
          Name "failure2"
        ]

  updateBranch :: Name -> MockUpdateI UpdateResult
  updateBranch nm@(Name "success1") = lift $ pure $ Success nm
  updateBranch nm@(Name "success2") = lift $ pure $ Success nm
  updateBranch nm@(Name "noChange1") = lift $ pure $ NoChange nm
  updateBranch nm@(Name "noChange2") = lift $ pure $ NoChange nm
  updateBranch nm@(Name "failure1") = lift $ pure $ Failure nm
  updateBranch nm@(Name "failure2") = lift $ pure $ Failure nm
  updateBranch _ = error "Mock MonadUpdateBranches is missing a pattern!"

  pushBranches :: MockUpdateI [UpdateResult]
  pushBranches = do
    Env {push} <- ask
    lift $ pure $ fmap nameToResult push

  checkoutCurrent :: CurrentBranch -> MockUpdateI ()
  checkoutCurrent (Name nm) = logInfoN $ "Checked out " <> nm

instance MonadLogger MockUpdateI where
  monadLoggerLog a b c d = MockUpdateT $ lift $ monadLoggerLog a b c d

nameToResult :: Name -> UpdateResult
nameToResult = Success