{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Core.MonadGit
( MonadGit(..)
, runWithReader
) where

import           Control.Monad.Reader (MonadReader, ask)
import           Control.Concurrent.ParallelIO.Global (parallelE)
import           Data.Kind (Type)
import qualified Data.Text as Txt

import Core.IO
import Types.Branch
import Types.Env
import Types.Error
import Types.GitTypes
import Types.ResultsWithErrs

class Monad m => MonadGit m where
  type UtilsType (m :: Type -> Type) (a :: Type)
    = (r :: Type) | r -> m a
  
  type UtilsResult (m :: Type -> Type)
    = (r :: Type) | r -> m

  grepBranches :: Env -> m [Name]
  parseStaleBranches :: Env -> [Name] -> m [UtilsType m AnyBranch]
  collectResults :: [UtilsType m AnyBranch] -> m (UtilsResult m)
  display :: (UtilsResult m) -> m ()

instance MonadGit IO where
  type UtilsType IO a = Either Err a
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
    
    where exceptToErr (Left x) = Left $ GitLog $ Txt.pack (show x)
          exceptToErr (Right r) = r

  collectResults :: [Either Err AnyBranch] -> IO ResultsWithErrs
  collectResults = return . toResultsWithErrs

  display :: ResultsWithErrs -> IO ()
  display = print

runWithReader :: (MonadGit m, MonadReader Env m) => m ()
runWithReader = do
  env <- ask
  branchNames <- grepBranches env

  staleBranches <- parseStaleBranches env branchNames

  res <- collectResults staleBranches

  display res