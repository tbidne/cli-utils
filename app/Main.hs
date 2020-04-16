{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import App
import Common.MonadLogger
import Common.Parsing
import Control.Concurrent.ParallelIO.Global
import qualified Control.Monad.Reader as R
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Git.FastForward.Core.MonadUpdateBranches
import qualified Git.FastForward.Parsing as FF
import Git.Stale.Core.MonadFindBranches
import qualified Git.Stale.Parsing as Stale
import Git.Stale.Types.Env ()
import System.Environment (getArgs)

currDay :: IO Day
currDay = fmap utctDay getCurrentTime

main :: IO ()
main = do
  args <- getArgs
  parseCmdAndRun args

parseCmdAndRun :: [String] -> IO ()
parseCmdAndRun [] = logError "No command. See --help for more info"
parseCmdAndRun (x : xs)
  | x `elem` ["fast-forward", "ff"] = run runUpdateBranches (FF.parseArgs xs)
  | x `elem` ["find-stale", "fs"] = do
    d <- currDay
    run runFindBranches (Stale.parseArgs d xs)
    stopGlobalPool
  | x `elem` ["--help", "-h"] = putStrLn help
  | otherwise = logError "Bad command. See --help for more info"

run ::
  AppT env IO () ->
  Either ParseErr env ->
  IO ()
run app eitherEnv =
  case eitherEnv of
    Left (Help h) -> putStrLn h
    Left (Err arg) -> logError $ "Could not parse `" <> T.pack arg <> "`. Try --help."
    Right env -> R.runReaderT (runAppT app) env

help :: String
help =
  "\nUsage: git-utils [CMD] [OPTIONS]\n\n"
    <> "Wrapper for executing Git actions.\n\nCommands:\n"
    <> " fast-forward, ff\tFast-forwards all local branches.\n\n"
    <> " find-stale, fs\t\tFinds stale branches.\n\n"
