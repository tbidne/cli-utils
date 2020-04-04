{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.MockUpdateBranches where

import Control.Monad.Reader
import Git.FastForward.Core.MonadUpdateBranches
import Git.FastForward.Types.Env
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes
import Output

newtype MockUpdateT m a = MockUpdateT {runMockUpdateT :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Env)

type MockUpdateOut = MockUpdateT Output


instance MonadUpdateBranches MockUpdateOut where
  fetch :: MockUpdateOut ()
  fetch = lift $ putShowable ("Fetching" :: String)

  getBranches :: MockUpdateOut LocalBranches
  getBranches =
    lift $ pure $
      LocalBranches
        (Name "current")
        [ Name "success1",
          Name "success2",
          Name "noChange1",
          Name "noChange2",
          Name "failure1",
          Name "failure2"
        ]

  updateBranch :: Name -> MockUpdateOut UpdateResult
  updateBranch nm@(Name "success1") = lift $ pure $ Success nm
  updateBranch nm@(Name "success2") = lift $ pure $ Success nm
  updateBranch nm@(Name "noChange1") = lift $ pure $ NoChange nm
  updateBranch nm@(Name "noChange2") = lift $ pure $ NoChange nm
  updateBranch nm@(Name "failure1") = lift $ pure $ Failure nm
  updateBranch nm@(Name "failure2") = lift $ pure $ Failure nm
  updateBranch _ = error "Mock MonadUpdateBranches is missing a pattern!"

  summarize :: [UpdateResult] -> MockUpdateOut ()
  summarize = lift . putShowList

  pushBranches :: MockUpdateOut [UpdateResult]
  pushBranches = do
    Env {push} <- ask
    lift $ pure $ fmap nameToResult push

  checkoutCurrent :: CurrentBranch -> MockUpdateOut ()
  checkoutCurrent (Name nm) = lift $ putShowable ("Checked out " <> nm)

nameToResult :: Name -> UpdateResult
nameToResult = Success