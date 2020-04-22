{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Common.MonadLogger
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides logging typeclass and functions.
module Common.MonadLogger
  ( MonadLogger (..),
    clearLine,
    logEmpty,
    resetCR,
    logError,
    logDebug,
    logInfo,
    logInfoBlue,
    logInfoCyan,
    logInfoSuccess,
    logWarn,
  )
where

import App
import qualified Control.Monad.Trans as MTL
import qualified Data.Text as T
import qualified System.Console.Pretty as P
import qualified System.IO as IO

-- | Represents a monad that can log 'T.Text'.
class Monad m => MonadLogger m where
  -- | Logs without a newline character.
  logNoLine :: T.Text -> m ()

  -- | Logs with a newline character.
  logLine :: T.Text -> m ()
  logLine = logNoLine . (<>) "\n"

instance MonadLogger IO where
  logNoLine txt = putStr (T.unpack txt) *> IO.hFlush IO.stdout
  logLine = putStrLn . T.unpack

instance MonadLogger m => MonadLogger (AppT env m) where
  logNoLine = MTL.lift . logNoLine
  logLine = MTL.lift . logLine

-- | 'resetCR' then `logLine` with 60 spaces.
clearLine :: MonadLogger m => m ()
clearLine = do
  resetCR
  logLine "                                                            "

-- | 'logLine' with the empty string.
logEmpty :: MonadLogger m => m ()
logEmpty = logLine ""

-- | 'logNoLine' with a carriage return.
resetCR :: MonadLogger m => m ()
resetCR = logNoLine "\r"

-- | Debug formatted 'logLine'.
logDebug :: MonadLogger m => T.Text -> m ()
logDebug = logLine . (<>) "[Debug] "

-- | Info formatted 'logLine'.
logInfo :: MonadLogger m => T.Text -> m ()
logInfo = logLine . (<>) "[Info] "

-- | Blue Info formatted 'logLine'.
logInfoBlue :: MonadLogger m => T.Text -> m ()
logInfoBlue = logLine . P.color P.Blue . (<>) "[Info] "

-- | Cyan Info formatted 'logLine'.
logInfoCyan :: MonadLogger m => T.Text -> m ()
logInfoCyan = logLine . P.color P.Cyan . (<>) "[Info] "

-- | Success Info formatted 'logLine'.
logInfoSuccess :: MonadLogger m => T.Text -> m ()
logInfoSuccess = logLine . P.color P.Green . (<>) "[Info] "

-- | Warn formatted 'logLine'.
logWarn :: MonadLogger m => T.Text -> m ()
logWarn = logLine . P.color P.Magenta . (<>) "[Warn] "

-- | Error formatted 'logLine'.
logError :: MonadLogger m => T.Text -> m ()
logError = logLine . P.color P.Red . (<>) "[Error] "
