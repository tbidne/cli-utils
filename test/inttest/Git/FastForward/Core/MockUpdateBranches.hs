{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.MockUpdateBranches
  ( MonadUpdateBranches (..),
  )
where

import Common.MonadLogger
import Git.FastForward.Core.MonadUpdateBranches
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.MergeType
import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes
import Output

instance MonadUpdateBranches Output where
  fetch :: Maybe FilePath -> Output ()
  fetch _ = logInfo "That's so fetch"

  getBranches :: Maybe FilePath -> Output LocalBranches
  getBranches _ =
    pure $
      LocalBranches
        (Name "current")
        [ Name "success1",
          Name "success2",
          Name "noChange1",
          Name "noChange2",
          Name "failure1",
          Name "failure2"
        ]

  updateBranch :: Maybe FilePath -> MergeType -> Name -> Output UpdateResult
  updateBranch _ _ nm@(Name "success1") = pure $ Success nm
  updateBranch _ _ nm@(Name "success2") = pure $ Success nm
  updateBranch _ _ nm@(Name "noChange1") = pure $ NoChange nm
  updateBranch _ _ nm@(Name "noChange2") = pure $ NoChange nm
  updateBranch _ _ nm@(Name "failure1") = pure $ Failure nm
  updateBranch _ _ nm@(Name "failure2") = pure $ Failure nm
  updateBranch _ _ _ = error "Mock MonadUpdateBranches is missing a pattern!"

  pushBranches :: Maybe FilePath -> [Name] -> Output [UpdateResult]
  pushBranches _ branches = do
    pure $ fmap Success branches

  checkoutCurrent :: Maybe FilePath -> CurrentBranch -> Output ()
  checkoutCurrent _ (Name nm) = logInfo $ "Checked out " <> nm
