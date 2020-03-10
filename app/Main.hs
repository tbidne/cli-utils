{-# LANGUAGE LambdaCase #-}

module Main
  ( main,
  )
where

import App
import Control.Concurrent.ParallelIO.Global
import Control.Monad.Reader (runReaderT)
import Core.MonadStaleBranches
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Parsing.Core
import System.Environment (getArgs)
import Types.Env ()

currDay :: IO Day
currDay = fmap utctDay getCurrentTime

main :: IO ()
main = do
  args <- getArgs
  d <- currDay
  case parseArgs d args of
    Left err -> putStrLn err
    Right env -> do
      runReaderT (runAppT runGitUtils) env
      stopGlobalPool
