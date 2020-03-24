{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Git.Stale.Core.FindBranches
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports functions to be used by "Core.FindBranches" for `IO`.
module Git.Stale.Core.IO
  ( errTupleToBranch,
    logIfErr,
    nameToLog,
    sh,
  )
where

import qualified Control.Exception as Ex
import Git.Stale.Core.Internal
import qualified Data.Text as T
import Git.Stale.Types.Branch
import Git.Stale.Types.Error
import Git.Types.GitTypes
import Git.CommonIO

-- | Retrieves the log information for a given branch `Name`
-- on `FilePath`. Returns `Left` `Err` if any errors occur,
-- `Right` `NameAuthDay` otherwise.
nameToLog :: Maybe FilePath -> ErrOr Name -> IO (ErrOr NameAuthDay)
nameToLog _ (Left err) = pure $ Left err
nameToLog path (Right name) = parseLog <$> getNameLog path name

-- | Maps a `NameAuthDay` on `FilePath` to `AnyBranch`. Returns `Left` `Err`
-- if any errors occur, `Right` `NameAuthDay` otherwise.
errTupleToBranch :: Maybe FilePath -> T.Text -> ErrOr NameAuthDay -> IO (ErrOr AnyBranch)
errTupleToBranch _ _ (Left x) = pure $ Left x
errTupleToBranch path master (Right (n, a, d)) = do
  res <- isMerged path master n
  pure $ Right $ mkAnyBranch n a d res

getNameLog :: Maybe FilePath -> Name -> IO NameLog
getNameLog p nm@(Name n) = sequenceA (nm, sh cmd p)
  where
    cmd =
      T.concat
        ["git log ", "\"", n, "\"", " --pretty=format:\"%an|%ad\" --date=short -n1"]

isMerged :: Maybe FilePath -> T.Text -> Name -> IO Bool
isMerged path master (Name n) = do
  res <-
    sh
      (T.concat ["git rev-list --count " <> master <> "..", "\"", n, "\""])
      path
  (return . (== 0) . unsafeToInt) res

-- | Runs an `IO` action and logs the error if any occur.
logIfErr :: forall a. IO a -> IO a
logIfErr io = do
  res <- Ex.try io :: IO (Either Ex.SomeException a)
  case res of
    Left ex -> do
      putStrLn $ "Died with error: " <> show ex
      io
    Right r -> pure r
