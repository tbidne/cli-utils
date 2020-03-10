{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Core.MockUtils where

import Control.Monad.Reader
import Core.MonadStaleBranches
import qualified Data.Text as Txt
import Types.Branch
import Types.Env
import Types.GitTypes

data Output a = Output [Txt.Text] a
  deriving (Eq, Ord, Show, Functor)

instance Applicative Output where
  pure :: a -> Output a
  pure = Output []

  (<*>) :: Output (a -> b) -> Output a -> Output b
  (Output rs f) <*> (Output ts x) = Output (rs <> ts) (f x)

instance Monad Output where
  (>>=) :: Output a -> (a -> Output b) -> Output b
  (Output rs x) >>= f = Output (rs <> ts) y where (Output ts y) = f x

putOutput :: Show a => [a] -> Output ()
putOutput xs = Output (fmap (Txt.pack . show) xs) ()

prependOut :: [Txt.Text] -> Output a -> Output a
prependOut ts (Output rs x) = Output (ts <> rs) x

newtype MockUtilsT m a = MockUtilsT {runMockUtilsT :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Env)

type MockUtilsOut = MockUtilsT Output

instance MonadStaleBranches MockUtilsOut where
  type Handler MockUtilsOut a = a
  type FinalResults MockUtilsOut = [AnyBranch]

  branchNamesByGrep :: MockUtilsOut [Name]
  branchNamesByGrep = do
    grep <- asks grepStr
    let maybeFilter = case grep of
          Nothing -> id
          Just s -> filter (\(Name n) -> s `Txt.isInfixOf` n)
    lift $ return $ maybeFilter allBranches

  getStaleLogs :: [Name] -> MockUtilsOut (Filtered NameAuthDay)
  getStaleLogs ns = do
    let removeStale ((Name n), _, _) = not $ "stale" `Txt.isInfixOf` n
        toLog nm@(Name n) = (nm, Author n, error "MockUtils -> getStaleLogs: day not defined")
    lift $ return $ (mkFiltered removeStale . fmap toLog) ns

  toBranches :: Filtered NameAuthDay -> MockUtilsOut [AnyBranch]
  toBranches = lift . return . fmap toBranch . unFiltered
    where
      toBranch (n, a, d) = mkAnyBranch n a d True

  collectResults :: [AnyBranch] -> MockUtilsOut [AnyBranch]
  collectResults = lift . return

  display :: [AnyBranch] -> MockUtilsOut ()
  display = MockUtilsT . lift . putOutput

addMockOut :: [Txt.Text] -> MockUtilsT Output a -> MockUtilsT Output a
addMockOut ts = MockUtilsT . mapReaderT (prependOut ts) . runMockUtilsT

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
