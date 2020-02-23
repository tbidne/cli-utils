{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module MockUtils where

import Control.Exception (SomeException, try)
import Control.Monad
import qualified Data.Text as Txt
import           Data.Time.Calendar (Day, diffDays, fromGregorian)
import           System.Process (readCreateProcess, shell)

import Branch
import Env
import Error
import GitTypes
import GitUtils
import Results


-- Prob just gonna use this in testing
-- generate strings by random, test parsing utils

data MockIO a = MockIO Txt.Text a
  deriving (Eq, Ord, Show, Functor)

instance Applicative MockIO where
  pure = MockIO ""
  (MockIO s f) <*> (MockIO t x) = MockIO (s <> t) (f x)

instance Monad MockIO where
  return = pure
  (MockIO s x) >>= f = MockIO (s <> t) y
    where (MockIO t y) = f x

newtype Wrap a = Wrap a
  deriving (Eq, Ord, Show, Functor)

instance Applicative Wrap where
  pure = Wrap
  (Wrap f) <*> (Wrap x) = Wrap (f x)

instance Monad Wrap where
  return = pure
  (Wrap x) >>= f = f x

instance GitUtils MockIO where
  type UtilsResult MockIO = Wrap

  grepBranches :: Txt.Text -> MockIO (Wrap [Name])
  grepBranches _ = inject mockBranchNames

  isMerged :: Name -> MockIO (Wrap Bool)
  isMerged (Name n) = inject $ (even . Txt.length) n

  logAuthDate :: Name -> MockIO (Wrap NameLog)
  logAuthDate nm@(Name n) = inject (nm, Log "sam|01-04-2020")

  collectResults :: Env -> MockIO Results
  collectResults Env{..} = do -- do
    --branchNames <- grepBranches grepStr -- Either Err [Name]

    branchNames <- grepBranches grepStr

    --logs <- logBranches branchNames -- [Either Err NameLog]

    --logs <- fmap logBranches branchNames

    --branches <- (parseIOBranches . filterIOLog limit today . parseIOLog) logs -- [Either Err AnyBranch]

    --return $ toResults branches

    undefined

  display :: Results -> MockIO ()
  display = undefined

inject :: a -> MockIO (Wrap a)
inject = return . return

mockBranchNames :: [Name]
mockBranchNames = [Name "origin/master", Name "fake"]

toInt :: Txt.Text -> Int
toInt = read . Txt.unpack