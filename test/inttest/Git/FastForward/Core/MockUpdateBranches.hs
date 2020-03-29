{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.MockUpdateBranches where

import Control.Monad.Reader
import Git.FastForward.Core.UpdateBranches
import Git.FastForward.Types.Env
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes
import Output

newtype MockUpdateBranchesT m a = MockUpdateBranchesT {runMockUpdateBranchesT :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Env)

type MockUpdateBranchesOut = MockUpdateBranchesT Output

instance UpdateBranches MockUpdateBranchesOut where
  fetch :: MockUpdateBranchesOut ()
  fetch = lift $ putSingleOutput ("Fetching" :: String)

  getBranches :: MockUpdateBranchesOut LocalBranches
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

  updateBranch :: Name -> MockUpdateBranchesOut UpdateResult
  updateBranch nm@(Name "success1") = lift $ pure $ Success nm
  updateBranch nm@(Name "success2") = lift $ pure $ Success nm
  updateBranch nm@(Name "noChange1") = lift $ pure $ NoChange nm
  updateBranch nm@(Name "noChange2") = lift $ pure $ NoChange nm
  updateBranch nm@(Name "failure1") = lift $ pure $ Failure nm
  updateBranch nm@(Name "failure2") = lift $ pure $ Failure nm
  updateBranch _ = error "Mock UpdateBranches is missing a pattern!"

  summarize :: [UpdateResult] -> MockUpdateBranchesOut ()
  summarize = lift . putOutput

  checkoutCurrent :: CurrentBranch -> MockUpdateBranchesOut ()
  checkoutCurrent (Name nm) = lift $ putSingleOutput ("Checked out " <> nm)
