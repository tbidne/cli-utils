module Main
  ( main,
  )
where

import App
import Control.Concurrent.ParallelIO.Global
import Control.Monad.Reader (runReaderT)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Git.FastForward.Core.UpdateBranches
import qualified Git.FastForward.Parsing as FF
import Git.Stale.Core.FindBranches
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
parseCmdAndRun (x:xs)
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
help = "Wrapper for executing Git actions. Valid commands:\n\n"
  <> "\tfast-forward, ff: Fast-forwards all local branches. Arguments:\n\n"
  <> "\t\t--path=<string>\n"
  <> "\t\t\tDirectory path, defaults to current directory.\n\n"
  <> "\t\t--merge-type=<u[pstream]|m[master]|<any>>\n"
  <> "\t\t\tThe branch to merge, defaults to upstream.\n"
  <> "\t\t\tMerges @{u}, origin/master, or <any> branch, respectively.\n"
  <> "\t\t\tAll options are merged with --ff-only.\n\n"
  <> "\tfind-stale, fs: Finds stale branches. Arguments:\n\n"
  <> "\t\t--grep=<string>\n"
  <> "\t\t\tString to filter branches on, defaults to the empty string.\n\n"
  <> "\t\t--path=<string>\n"
  <> "\t\t\tDirectory path, defaults to current directory.\n\n"
  <> "\t\t--limit=<days>\n"
  <> "\t\t\tStale threshold. Defaults to 30, any non-negative integer is valid.\n\n"
  <> "\t\t--branch-type=<a[ll]|r[emote]|l[ocal]>\n"
  <> "\t\t\tDetermines the branches to search, defaults to remote.\n\n"
  <> "\t\t--remote=<string>\n"
  <> "\t\t\tSpecifies the name of the remote, for stripping during display purposes.\n"
  <> "\t\t\tDefaults to origin.\n\n"
  <> "\t\t--master=<string>\n"
  <> "\t\t\tName of the branch to consider merges against. Defaults to origin/master.\n"