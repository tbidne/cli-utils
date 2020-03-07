{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.IO
  ( parseLog
  , parseAuthDateStr
  , parseDay
  , getNameLog
  , nameToLog
  , isMerged
  , errTupleToBranch
  , logIfErr
  , sh
  )
where

import qualified Control.Exception as Ex
import qualified Data.Text as T
import qualified System.Process as P

import           Core.Internal
import           Types.Branch
import           Types.Env
import           Types.Error
import           Types.GitTypes

nameToLog :: Env -> Name -> IO (ErrOr NameAuthDay)
nameToLog Env {..} name = parseLog <$> getNameLog path name

errTupleToBranch :: Env -> ErrOr NameAuthDay -> IO (ErrOr AnyBranch)
errTupleToBranch _   (Left  x        ) = return $ Left x
errTupleToBranch env (Right (n, a, d)) = do
  res <- isMerged env n
  return $ Right $ mkAnyBranch n a d res

getNameLog :: Maybe FilePath -> Name -> IO NameLog
getNameLog p nm@(Name n) = sequenceA (nm, sh cmd p)
 where
  cmd = T.concat
    ["git log ", "\"", n, "\"", " --pretty=format:\"%an|%ad\" --date=short -n1"]

isMerged :: Env -> Name -> IO Bool
isMerged Env {..} (Name n) = do

  res <- sh
    (T.concat ["git rev-list --count origin/master..", "\"", n, "\""])
    path

  (return . (== 0) . toInt) res

sh :: T.Text -> Maybe FilePath -> IO T.Text
sh cmd fp = T.pack <$> P.readCreateProcess proc ""
  where proc = (P.shell (T.unpack cmd)) { P.cwd = fp }

logIfErr :: forall a . IO a -> IO a
logIfErr io = do
  res <- Ex.try io :: IO (Either Ex.SomeException a)
  case res of
    Left ex -> do
      putStrLn $ "Died with error: " <> show ex
      io
    Right r -> return r
