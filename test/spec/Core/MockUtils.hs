{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Core.MockUtils where

import qualified Data.Text as Txt

import Core.MonadGit
import Types.Branch
import Types.Env
import Types.GitTypes

newtype Wrap a = Wrap { unWrap :: a }
  deriving (Eq, Ord, Show, Functor)

data MockUtils a = MockUtils { output :: [Txt.Text], unMock :: a }
  deriving (Eq, Ord, Show, Functor)

instance Applicative MockUtils where
  pure :: a -> MockUtils a
  pure = MockUtils []

  (<*>) :: MockUtils (a -> b) -> MockUtils a -> MockUtils b
  (MockUtils rs f) <*> (MockUtils ts x) = MockUtils (rs <> ts) (f x)

instance Monad MockUtils where
  (>>=) :: MockUtils a -> (a -> MockUtils b) -> MockUtils b
  (MockUtils rs x) >>= f = MockUtils (rs <> ts) y
    where (MockUtils ts y) = f x

instance MonadGit MockUtils where
  type UtilsType MockUtils = Wrap
  type UtilsResult MockUtils = [AnyBranch]

  grepBranches :: Env -> MockUtils [Name]
  grepBranches Env{..} = MockUtils [] (maybeFilter allBranches)
    where maybeFilter = case grepStr of
            Nothing -> id
            Just s  -> filter (\(Name n) -> s `Txt.isInfixOf` n)

  parseStaleBranches :: Env -> [Name] -> MockUtils [Wrap AnyBranch]
  parseStaleBranches Env{..} = MockUtils [] . fmap (Wrap . toBranch)
    where toBranch nm@(Name n) = mkAnyBranch nm (Author n) today True

  collectResults :: [Wrap AnyBranch] -> MockUtils [AnyBranch]
  collectResults = MockUtils [] . fmap unWrap

  display :: [AnyBranch] -> MockUtils ()
  display bs = MockUtils (fmap f bs) ()
    where f = Txt.pack . show

allBranches :: [Name]
allBranches = [ Name "branch 1"
               , Name "branch 2"
               , Name "branch 3"
               , Name "branch 4"
               , Name "branch 5"
               , Name "other 1"
               , Name "other 2"
               , Name "other 3" ]