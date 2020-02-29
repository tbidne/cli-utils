{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module GitUtils.Core
( GitUtils(..)
, runWithReader
, parseAuthDateStr
, parseDay
, stale
) where

import           Control.Exception (SomeException)
import           Control.Monad.Reader (MonadReader, ask)
import           Control.Concurrent.ParallelIO.Global (parallelE)
import           Data.Kind (Type)
import qualified Data.Text as Txt

import GitUtils.Internal
import GitUtils.IO
import Types.Branch
import Types.Env
import Types.Error
import Types.GitTypes
import Types.ResultsWithErrs

class Monad m => GitUtils m where
  type UtilsType m :: Type -> Type
  type UtilsResult m :: Type

  grepBranches :: Env -> m [Name]
  parseStaleBranches :: Env -> [Name] -> m [UtilsType m AnyBranch]
  collectResults :: [UtilsType m AnyBranch] -> m (UtilsResult m)
  display :: (UtilsResult m) -> m ()

runWithReader :: (GitUtils m, MonadReader Env m) => m ()
runWithReader = do
  env <- ask
  branchNames <- grepBranches env

  staleBranches <- parseStaleBranches env branchNames

  res <- collectResults staleBranches

  display res

instance GitUtils IO where
  type UtilsType IO = Either Err
  type UtilsResult IO = ResultsWithErrs

  grepBranches :: Env -> IO [Name]
  grepBranches Env{..} = do
    res <- sh "git branch -r" path

    logIfErr $ return $ toNames res

    where maybeFilter = case grepStr of
            Nothing -> id
            Just s  -> filter (\t -> (Txt.toCaseFold s) `Txt.isInfixOf` (Txt.toCaseFold t))
          toNames = fmap (Name . Txt.strip) . maybeFilter . Txt.lines

  parseStaleBranches :: Env -> [Name] -> IO [Either Err AnyBranch]
  parseStaleBranches env@Env{..} ns = do
    branches <- parallelE (fmap (nameToBranch env) ns)

    return $ fmap exceptToErr branches
    
    where exceptToErr :: Either SomeException (Either Err AnyBranch) -> Either Err AnyBranch
          exceptToErr (Left x) = Left $ GitLog $ Txt.pack (show x)
          exceptToErr (Right r) = r

  collectResults :: [Either Err AnyBranch] -> IO ResultsWithErrs
  collectResults = return . toResultsWithErrs

  display :: ResultsWithErrs -> IO ()
  display = print