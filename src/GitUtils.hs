{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GitUtils
( GitUtils(..)
, runWithReader
, parseAuthDateStr
, parseDay
, parseLog
, stale
) where

import           Control.Exception (SomeException, try)
import           Control.Monad
import           Control.Monad.Reader (MonadReader, ask)
import           Control.Concurrent.ParallelIO.Global (parallel, parallelE)
import           Data.Kind (Type)
import qualified Data.Text as Txt
import           Data.Time.Calendar (Day, diffDays, fromGregorian)
import           System.Process (readCreateProcess, shell, cwd)

import Branch
import Env
import Error
import GitTypes
import ResultsWithErrs

class Monad m => GitUtils m where
  type UtilsType m :: Type -> Type
  type UtilsResult m :: Type

  grepBranches :: Env -> m [Name]
  logAuthDate :: Env -> [Name] -> m [UtilsType m BranchLog]
  filterFreshBranches :: Env -> [UtilsType m BranchLog] -> m [UtilsType m BranchLog]
  isMerged :: Env -> Name -> m (UtilsType m Bool)
  toAnyBranch :: Env -> [UtilsType m BranchLog] -> m [UtilsType m AnyBranch]
  collectResults :: [UtilsType m AnyBranch] -> m (UtilsResult m)
  display :: (UtilsResult m) -> m ()

runWithReader :: (GitUtils m, MonadReader Env m) => m ()
runWithReader = do
  env <- ask
  branchNames <- grepBranches env
  
  logs <- (logAuthDate env) branchNames

  staleLogs <- (filterFreshBranches env) logs

  staleBranches <- (toAnyBranch env) staleLogs

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
            Just s  -> filter $ Txt.isInfixOf s
          toNames = fmap (Name . Txt.strip) . maybeFilter . Txt.lines

  logAuthDate :: Env -> [Name] -> IO [Either Err BranchLog]
  logAuthDate Env{..} ns = do
    logs <- (parallelE (fmap (getLog path) ns)) :: IO [Either SomeException (Name, Txt.Text)]

    return $ fmap parseErrAndRes logs

    where getLog p nm@(Name n) = sequenceA (nm, sh (Txt.concat ["git log ", n, " --pretty=format:\"%an|%ad\" --date=short -n1"]) p)

          parseErrAndRes :: Either SomeException (Name, Txt.Text) -> Either Err BranchLog
          parseErrAndRes (Left x) = (Left . GitLog . Txt.pack . show) x
          parseErrAndRes (Right (n, l)) = parseLog (n, Log l)

  filterFreshBranches :: Env -> [Either Err BranchLog] -> IO [Either Err BranchLog]
  filterFreshBranches Env{..} = return . filter f
    where f (Left _) = True
          f (Right bl) = stale limit today bl

  isMerged :: Env -> Name -> IO (Either Err Bool)
  isMerged Env{..} (Name n) = do

    res <- sh (Txt.concat ["git rev-list --count origin/master..", n]) path

    (wrapErr GitMerge . return . (== 0) . toInt) res

  toAnyBranch :: Env -> [Either Err BranchLog] -> IO [Either Err AnyBranch]
  toAnyBranch env = parallel . fmap f
    where f (Left x) = return $ Left x
          f (Right (BranchLog (n, a, d))) = do
            res <- isMerged env n
            return $ case res of
              Left err -> Left err
              Right b -> Right $ mkAnyBranch n a d b

  collectResults :: [Either Err AnyBranch] -> IO ResultsWithErrs
  collectResults = return . toResultsWithErrs

  display :: ResultsWithErrs -> IO ()
  display = print

stale :: Integer -> Day -> BranchLog -> Bool
stale lim day (BranchLog (_, _, d)) = diffDays day d > lim

parseLog :: NameLog -> Either Err BranchLog
parseLog = parseAuthDateStr >=> parseDay

parseAuthDateStr :: NameLog -> Either Err NameAuthDateStr
parseAuthDateStr (n, (Log l)) =
  case Txt.splitOn "|" l of
    [a, t] -> Right (n, Author a, t)
    _      -> Left $ ParseLog l

parseDay :: NameAuthDateStr -> Either Err BranchLog
parseDay (n, a, t) =
  case fmap toInt (Txt.splitOn "-" t) of
    [y, m, d] -> Right $ BranchLog (n, a, fromGregorian (toInteger y) m d)
    _         -> Left $ ParseDate t

sh :: Txt.Text -> Maybe FilePath -> IO Txt.Text
sh cmd fp = Txt.pack <$> readCreateProcess proc ""
  where proc = (shell (Txt.unpack cmd)) { cwd = fp }

logIfErr :: forall a. IO a -> IO a
logIfErr io = do
  res <- try io :: IO (Either SomeException a)
  case res of
    Left ex -> do
      putStrLn $ "Died with error: " <> show ex
      io
    Right r -> return r

wrapErr :: forall a. (Txt.Text -> Err) -> IO a -> IO (Either Err a)
wrapErr errFn io = do
  res <- try io :: IO (Either SomeException a)
  return $ case res of
    Left ex -> Left $ (errFn . Txt.pack . show) ex
    Right r -> return r

toInt :: Txt.Text -> Int
toInt = read . Txt.unpack