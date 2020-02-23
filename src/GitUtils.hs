{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module GitUtils
( GitUtils(..)
, parseAuthDateStr
, parseDay
, parseLog
, parseMerged
, stale
) where

import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.Kind (Type)
import qualified Data.Text as Txt
import           Data.Time.Calendar (Day, diffDays, fromGregorian)
import qualified System.Process as P

import Branch
import Env
import Error
import GitTypes
import Results

class Monad m => GitUtils m where
  type UtilsResult m :: Type -> Type

  grepBranches :: Env -> m (UtilsResult m [Name])
  isMerged :: Env -> Name -> m (UtilsResult m Bool)
  logAuthDate :: Env -> Name -> m (UtilsResult m NameLog)
  collectResults :: Env -> m Results
  display :: Results -> m ()

instance GitUtils IO where
  type UtilsResult IO = Either Err

  grepBranches :: Env -> IO (Either Err [Name])
  grepBranches Env{..} = wrapErr GitBranches res
    where cmd = case grepStr of
                  Nothing -> "git branch -r"
                  Just s  -> Txt.concat ["git branch -r | grep ", s, " -i"]
          res = fmap (fmap Name . fmap Txt.strip . Txt.lines) (sh cmd path)

  isMerged :: Env -> Name -> IO (Either Err Bool)
  isMerged Env{..} (Name n) = wrapErr GitMerge res
    where cmd = Txt.concat ["git rev-list --count origin/master..", n]
          res = fmap ((== 0) . toInt) (sh cmd path)
          --res = fmap (\_ -> True) (sh cmd path)

  logAuthDate :: Env -> Name -> IO (Either Err NameLog)
  logAuthDate Env{..} nm@(Name n) = wrapErr GitLog res
    where cmd = Txt.concat ["git log ", n, " --pretty=format:\"%an|%ad\" --date=short -n1"]
          res = fmap (\l -> (nm, Log l)) (sh cmd path)

  collectResults :: Env -> IO Results
  collectResults env@Env{..} = do
    branchNames <- grepBranches env

    logs <- logBranchesIO env branchNames

    branches <- (parseBranchesIO env . filterErrLog limit today . parseErrLog) logs

    return $ toResults branches

  display :: Results -> IO ()
  display = print

stale :: Integer -> Day -> NameAuthDay -> Bool
stale lim day (_, _, d) = diffDays day d > lim

parseLog :: NameLog -> Either Err NameAuthDay
parseLog = parseAuthDateStr >=> parseDay

parseAuthDateStr :: NameLog -> Either Err NameAuthDateStr
parseAuthDateStr (n, (Log l)) =
  case Txt.splitOn "|" l of
    [a, t] -> Right (n, Author a, t)
    _      -> Left $ ParseLog l

parseDay :: NameAuthDateStr -> Either Err NameAuthDay
parseDay (n, a, t) =
  case fmap toInt (Txt.splitOn "-" t) of
    [y, m, d] -> Right (n, a, fromGregorian (toInteger y) m d)
    _         -> Left $ ParseDate t

parseMerged :: (GitUtils m, Functor (UtilsResult m)) => Env -> NameAuthDay -> m (UtilsResult m AnyBranch)
parseMerged env (n, a, d) = (fmap . fmap) (mkAnyBranch n a d) (isMerged env n)

sh :: Txt.Text -> Maybe FilePath -> IO Txt.Text
sh cmd fp = Txt.pack <$> P.readCreateProcess proc ""
  where proc = (P.shell (Txt.unpack cmd)) { P.cwd = fp }

wrapErr :: forall a. (Txt.Text -> Err) -> IO a -> IO (Either Err a)
wrapErr errFn io = do
  res <- try io :: IO (Either SomeException a)
  return $ case res of
    Left ex -> Left $ (errFn . Txt.pack . show) ex
    Right r -> Right r

logBranchesIO :: Env -> Either Err [Name] -> IO [Either Err NameLog]
logBranchesIO env xs = fmap (fmap join . sequenceA) (traverse (traverse (logAuthDate env)) xs)

parseErrLog :: [Either Err NameLog] -> [Either Err NameAuthDay]
parseErrLog es = fmap (parseLog =<<) es

filterErrLog :: Integer -> Day -> [Either Err NameAuthDay] -> [Either Err NameAuthDay]
filterErrLog limit today = filter f
  where f (Left _) = True
        f (Right l) = stale limit today l

parseBranchesIO :: Env -> [Either Err NameAuthDay] -> IO [Either Err AnyBranch]
parseBranchesIO env = traverseJoin (parseMerged env)

traverseJoin :: (Monad m, Traversable m, Applicative n, Traversable f)
  => (a -> n (m b))
  -> f (m a)
  -> n (f (m b))
traverseJoin f xs = (fmap . fmap) join (traverse sequenceA ((fmap . fmap) f xs))

toInt :: Txt.Text -> Int
toInt = read . Txt.unpack