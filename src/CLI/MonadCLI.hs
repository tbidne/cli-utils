{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.MonadCLI
  ( MonadCLI (..),
    runCLI,
  )
where

import App
import CLI.Types.Env
import Common.IO
import Common.MonadLogger
import Common.Utils
import qualified Control.Concurrent.Async as A
import Control.Monad ((>=>))
import qualified Control.Monad.Loops as L
import qualified Control.Monad.Reader as R
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as May
import qualified Data.Text as T
import qualified System.Clock as C

class Monad m => MonadCLI m where
  runCommands :: [T.Text] -> m ()

instance MonadCLI IO where
  runCommands :: [T.Text] -> IO ()
  runCommands commands = do
    start <- C.getTime C.Monotonic
    actionAsync <- A.async $ A.mapConcurrently_ runCommand commands
    counter actionAsync
    end <- C.getTime C.Monotonic
    let totalTime = diffTime start end
    clearLine
    logInfoBlue "Finished!"
    logInfoBlue $ "Total time elapsed: " <> formatSeconds totalTime

instance MonadCLI m => MonadCLI (AppT Env m) where
  runCommands = R.lift . runCommands

runCLI :: (R.MonadReader Env m, MonadCLI m) => m ()
runCLI = do
  Env {legend, commands} <- R.ask
  let commands' = translateCommands legend commands
  runCommands commands'

runCommand :: T.Text -> IO ()
runCommand cmd = do
  res <- tryTimeSh cmd Nothing
  (seconds, logFn, msg) <- case res of
    Left (t, err) -> pure (t, logError, err)
    Right (t, _) -> pure (t, logInfoSuccess, "Successfully ran `" <> cmd <> "`")
  clearLine
  logFn msg
  logFn $ "Time elapsed: " <> formatSeconds seconds <> "\n"

counter :: A.Async a -> IO ()
counter asyn = do
  start <- C.getTime C.Monotonic
  L.whileM_ (unfinished asyn) $ do
    sh_ "sleep 1" Nothing
    elapsed <- C.getTime C.Monotonic
    let diff = diffTime start elapsed
    resetCR
    logNoLine $ "Running time: " <> formatSeconds diff

unfinished :: A.Async a -> IO Bool
unfinished = A.poll >=> pure . not . May.isJust

translateCommands :: Map.Map T.Text T.Text -> [T.Text] -> [T.Text]
translateCommands mp = foldMap (lineToCommands mp)

lineToCommands :: Map.Map T.Text T.Text -> T.Text -> [T.Text]
lineToCommands mp line =
  case T.splitOn "," line of
    [l] ->
      case Map.lookup l mp of
        Just c -> lineToCommands mp c
        Nothing -> [l]
    xs -> xs >>= lineToCommands mp