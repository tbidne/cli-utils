module Main
  ( main,
  )
where

import App
import Control.Concurrent.ParallelIO.Global
import Control.Monad.Reader (runReaderT)
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
parseCmdAndRun [] = putStrLn "No command. See --help for more info"
parseCmdAndRun (x : xs)
  | x `elem` ["fast-forward", "ff"] = run runUpdateBranches FF.parseArgs xs
  | x `elem` ["find-stale", "fs"] = do
    d <- currDay
    run runFindBranches (Stale.parseArgs d) xs
    stopGlobalPool
  | x `elem` ["--help", "-h"] = putStrLn help
  | otherwise = putStrLn "Bad command. See --help for more info"

run ::
  AppT env IO () ->
  ([String] -> Either String env) ->
  [String] ->
  IO ()
run appFn parseFn args =
  case parseFn args of
    Left err -> putStrLn err
    Right env -> runReaderT (runAppT appFn) env

help :: String
help =
  "\nUsage: git-utils [CMD] [OPTIONS]\n\n"
    <> "Wrapper for executing Git actions.\n\nCommands:\n"
    <> " fast-forward, ff\tFast-forwards all local branches.\n\n"
    <> " find-stale, fs\t\tFinds stale branches.\n\n"
