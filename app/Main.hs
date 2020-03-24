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
import System.IO
import System.IO.Silently

currDay :: IO Day
currDay = fmap utctDay getCurrentTime

main :: IO ()
main = do
  args <- getArgs
  parseCmdAndRun args

parseCmdAndRun :: [String] -> IO ()
parseCmdAndRun [] = error "No argument!"
parseCmdAndRun (x : xs) =
  case x of
    "fast-forward" -> run runUpdateBranches FF.parseArgs xs
    "find-stale" -> do
      d <- currDay
      run runFindBranches (Stale.parseArgs d) xs
      stopGlobalPool
    _ -> putStrLn "Bad argument. Supported commands are `find-stale` and `fast-forward`"

run ::
  AppT env IO () ->
  ([String] -> Either String env) ->
  [String] ->
  IO ()
run appFn parseFn args =
  case parseFn args of
    Left err -> putStrLn err
    Right env -> runReaderT (runAppT appFn) env
