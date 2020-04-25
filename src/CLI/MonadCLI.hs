{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : CLI.MonadCLI
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- The MonadCLI class.
module CLI.MonadCLI
  ( MonadCLI (..),
    runCLI,
  )
where

import App
import CLI.Internal
import CLI.Types.Env
import Common.IO
import Common.MonadLogger
import Common.Types.NonNegative
import Common.Utils
import qualified Control.Concurrent.Async as A
import Control.Monad ((>=>))
import qualified Control.Monad.Loops as L
import qualified Control.Monad.Reader as R
import qualified Data.Maybe as May
import qualified Data.Text as T
import qualified System.Clock as C

-- | The 'MonadCLI' class is used to describe running a list
-- of 'T.Text' commands.
class Monad m => MonadCLI m where
  -- | Runs the list of commands.
  runCommands :: [T.Text] -> Maybe (NonNegative Int) -> m ()

-- | `MonadCLI` instance for `IO`. Runs each command concurrently,
-- timing each one, and printing the outcome as success/failure.
instance MonadCLI IO where
  runCommands :: [T.Text] -> Maybe (NonNegative Int) -> IO ()
  runCommands commands timeout = do
    start <- C.getTime C.Monotonic
    actionAsync <- A.async $ A.mapConcurrently_ runCommand commands
    counter actionAsync timeout
    end <- C.getTime C.Monotonic
    let totalTime = diffTime start end :: NonNegative Integer
    clearLine
    logInfoBlue "Finished!"
    logInfoBlue $ "Total time elapsed: " <> formatSeconds totalTime

instance MonadCLI m => MonadCLI (AppT Env m) where
  runCommands cmds = R.lift . runCommands cmds

-- | High level logic of `MonadCLI` usage. This function is the
-- entrypoint for any `MonadCLI` instance.
runCLI :: (R.MonadReader Env m, MonadCLI m) => m ()
runCLI = do
  Env {legend, timeout, commands} <- R.ask
  let commands' = translateCommands legend commands
  runCommands commands' timeout

runCommand :: T.Text -> IO ()
runCommand cmd = do
  res <- tryTimeSh cmd Nothing
  (seconds :: NonNegative Integer, logFn, msg) <- case res of
    Left (t, err) -> pure (t, logError, err)
    Right (t, _) -> pure (t, logInfoSuccess, "Successfully ran `" <> cmd <> "`")
  clearLine
  logFn msg
  logFn $ "Time elapsed: " <> formatSeconds seconds <> "\n"

counter :: A.Async a -> Maybe (NonNegative Int) -> IO ()
counter asyn timeout = do
  start <- C.getTime C.Monotonic
  L.whileM_ (keepRunning asyn start timeout) $ do
    sh_ "sleep 1" Nothing
    elapsed <- C.getTime C.Monotonic
    let diff = diffTime start elapsed :: NonNegative Integer
    resetCR
    logNoLine $ "Running time: " <> formatSeconds diff

keepRunning :: A.Async a -> C.TimeSpec -> Maybe (NonNegative Int) -> IO Bool
keepRunning asyn start to = do
  running <- unfinished asyn
  currTime <- C.getTime C.Monotonic
  let hasTimedOut = timedOut start currTime to
  if running && hasTimedOut
    then do
      A.cancel asyn
      clearLine
      logWarn "Timed out, cancelling remaining tasks."
      pure False
    else pure running

timedOut :: C.TimeSpec -> C.TimeSpec -> Maybe (NonNegative Int) -> Bool
timedOut start curr =
  \case
    Nothing -> False
    Just t ->
      let timeSoFar = diffTime start curr
       in timeSoFar > t

unfinished :: A.Async a -> IO Bool
unfinished = A.poll >=> pure . May.isNothing
