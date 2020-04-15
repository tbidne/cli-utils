{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import App
import CLI.MonadCLI
import CLI.Parsing as CLI
import Common.MonadLogger
import Common.Parsing
import Control.Concurrent.ParallelIO.Global
import qualified Control.Monad.Reader as R
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Clock
import Git.FastForward.Core.MonadUpdateBranches
import qualified Git.FastForward.Parsing as FF
import Git.Stale.Core.MonadFindBranches
import qualified Git.Stale.Parsing as Stale
import Git.Stale.Types.Env ()
import qualified System.Environment as Sys

main :: IO ()
main = Sys.getArgs >>= parseCmdAndRun

currDay :: IO Cal.Day
currDay = fmap Clock.utctDay Clock.getCurrentTime

parseCmdAndRun :: [String] -> IO ()
parseCmdAndRun [] = logError "No command. See --help for more info"
parseCmdAndRun (x : xs)
  | x `elem` ["run-sh", "rs"] = do
    eitherEnv <- CLI.parseArgs xs
    run runCLI eitherEnv
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
run app =
  \case
    Left (Help h) -> putStrLn h
    Left (Err err) -> logError $ T.pack err
    Right env -> R.runReaderT (runAppT app) env

help :: String
help =
  "\nUsage: git-utils [CMD] [OPTIONS]\n\n"
    <> "Wrapper for executing Git actions.\n\nCommands:\n"
    <> " fast-forward, ff\tFast-forwards all local branches.\n\n"
    <> " find-stale, fs\t\tFinds stale branches.\n\n"
