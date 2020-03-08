{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.IO
  ( parseLog,
    parseAuthDateStr,
    parseDay,
    getNameLog,
    nameToLog,
    isMerged,
    errTupleToBranch,
    logIfErr,
    sh,
  )
where

import qualified Control.Exception as Ex
import Core.Internal
import qualified Data.Text as T
import qualified System.Process as P
import Types.Branch
import Types.Error
import Types.GitTypes

nameToLog :: Maybe FilePath -> Name -> IO (ErrOr NameAuthDay)
nameToLog path name = parseLog <$> getNameLog path name

errTupleToBranch :: Maybe FilePath -> ErrOr NameAuthDay -> IO (ErrOr AnyBranch)
errTupleToBranch _ (Left x) = return $ Left x
errTupleToBranch path (Right (n, a, d)) = do
  res <- isMerged path n
  return $ Right $ mkAnyBranch n a d res

getNameLog :: Maybe FilePath -> Name -> IO NameLog
getNameLog p nm@(Name n) = sequenceA (nm, sh cmd p)
  where
    cmd =
      T.concat
        ["git log ", "\"", n, "\"", " --pretty=format:\"%an|%ad\" --date=short -n1"]

isMerged :: Maybe FilePath -> Name -> IO Bool
isMerged path (Name n) = do
  res <-
    sh
      (T.concat ["git rev-list --count origin/master..", "\"", n, "\""])
      path
  (return . (== 0) . toInt) res

sh :: T.Text -> Maybe FilePath -> IO T.Text
sh cmd fp = T.pack <$> P.readCreateProcess proc ""
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

logIfErr :: forall a. IO a -> IO a
logIfErr io = do
  res <- Ex.try io :: IO (Either Ex.SomeException a)
  case res of
    Left ex -> do
      putStrLn $ "Died with error: " <> show ex
      io
    Right r -> return r
