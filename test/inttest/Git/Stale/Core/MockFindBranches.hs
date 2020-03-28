{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Git.Stale.Core.MockFindBranches where

import Output
import Control.Monad.Reader
import qualified Data.Text as Txt
import Git.Stale.Core.FindBranches
import Git.Stale.Types.Branch
import Git.Stale.Types.Env
import Git.Stale.Types.Filtered
import Git.Types.GitTypes
import Git.Types.Handler

newtype MockFindBranchesT m a = MockFindBranchesT {runMockFindBranchesT :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Env)

type MockFindBranchesOut = MockFindBranchesT Output

type instance Handler MockFindBranchesOut a = a

instance FindBranches MockFindBranchesOut where
  type FinalResults MockFindBranchesOut = [AnyBranch]

  branchNamesByGrep :: MockFindBranchesOut [Name]
  branchNamesByGrep = do
    grep <- asks grepStr
    let maybeFilter = case grep of
          Nothing -> id
          Just s -> filter (\(Name n) -> s `Txt.isInfixOf` n)
    lift $ pure $ maybeFilter allBranches

  getStaleLogs :: [Name] -> MockFindBranchesOut (Filtered NameAuthDay)
  getStaleLogs ns = do
    let removeStale ((Name n), _, _) = not $ "stale" `Txt.isInfixOf` n
        toLog nm@(Name n) = (nm, Author n, error "MockFindBranches -> getStaleLogs: day not defined")
    lift $ pure $ (mkFiltered removeStale . fmap toLog) ns

  toBranches :: Filtered NameAuthDay -> MockFindBranchesOut [AnyBranch]
  toBranches = lift . pure . fmap toBranch . unFiltered
    where
      toBranch (n, a, d) = mkAnyBranch n a d True

  collectResults :: [AnyBranch] -> MockFindBranchesOut [AnyBranch]
  collectResults = lift . return

  display :: [AnyBranch] -> MockFindBranchesOut ()
  display = MockFindBranchesT . lift . putOutput

addMockOut :: [Txt.Text] -> MockFindBranchesOut a -> MockFindBranchesOut a
addMockOut ts = MockFindBranchesT . mapReaderT (prependOut ts) . runMockFindBranchesT

allBranches :: [Name]
allBranches =
  [ Name "branch 1 stale",
    Name "branch 2 stale",
    Name "branch 3",
    Name "branch 4 stale",
    Name "branch 5",
    Name "branch 6",
    Name "branch 7",
    Name "branch 8",
    Name "other 1",
    Name "other 2 stale",
    Name "other 3",
    Name "other 4 stale",
    Name "other 5"
  ]
