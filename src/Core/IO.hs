{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.IO
( parseLog
, parseAuthDateStr
, parseDay
, nameToBranch
, getNameLog
, isMerged
, logIfErr
, sh
)
where

import qualified Data.Text as Txt
import           Control.Exception (SomeException, try)
import           System.Process (readCreateProcess, shell, cwd)

import Core.Internal
import Types.Branch
import Types.Env
import Types.Error
import Types.GitTypes

nameToBranch :: Env -> Name -> IO (Either Err AnyBranch)
nameToBranch env@Env{..} name = do
  nameLog <- getNameLog path name

  (logsToBranches . parseLog) nameLog
  where logsToBranches (Left x) = return $ Left x
        logsToBranches (Right (n, a, d)) = do
          res <- isMerged env n
          return $ Right $ mkAnyBranch n a d res

getNameLog :: Maybe FilePath -> Name -> IO (Name, Txt.Text)
getNameLog p nm@(Name n) = sequenceA (nm, sh (format n) p)
  where format x = Txt.concat
          [ "git log "
          , "\"" , x , "\""
          , " --pretty=format:\"%an|%ad\" --date=short -n1" ]

isMerged :: Env -> Name -> IO Bool
isMerged Env{..} (Name n) = do

  res <- sh (Txt.concat ["git rev-list --count origin/master..", "\"", n, "\""]) path

  (return . (== 0) . toInt) res

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