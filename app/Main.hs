{-# LANGUAGE LambdaCase #-}

module Main
  ( main,
  )
where

import App
import Control.Concurrent.ParallelIO.Global
import Control.Monad.Reader (runReaderT)
import Git.Stale.Core.FindBranches
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Git.Stale.Parsing.Core
import System.Environment (getArgs)
import System.IO
import System.IO.Silently
import Git.Stale.Types.Env ()

currDay :: IO Day
currDay = fmap utctDay getCurrentTime

main :: IO ()
main = hSilence [stderr] $ do
  args <- getArgs
  d <- currDay
  case parseArgs d args of
    Left err -> putStrLn err
    Right env -> do
      runReaderT (runAppT runGitUtils) env
      stopGlobalPool
