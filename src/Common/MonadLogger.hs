{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

class Monad m => MonadLogger m where
  logNoLine :: T.Text -> m ()

  logLine :: T.Text -> m ()
  logLine = logNoLine . (<>) "\n"

instance MonadLogger IO where
  logNoLine txt = putStr (T.unpack txt) *> IO.hFlush IO.stdout
  logLine = putStrLn . T.unpack

instance MonadLogger m => MonadLogger (AppT env m) where
  logNoLine = MTL.lift . logNoLine
  logLine = MTL.lift . logLine

clearLine :: MonadLogger m => m ()
clearLine = do
  resetCR
  logLine "                                                            "

logEmpty :: MonadLogger m => m ()
logEmpty = logLine ""

resetCR :: MonadLogger m => m ()
resetCR = logNoLine "\r"

logDebug :: MonadLogger m => T.Text -> m ()
logDebug = logLine . (<>) "[Debug] "

logInfo :: MonadLogger m => T.Text -> m ()
logInfo = logLine . (<>) "[Info] "

logInfoBlue :: MonadLogger m => T.Text -> m ()
logInfoBlue = logLine . P.color P.Blue . (<>) "[Info] "

logInfoCyan :: MonadLogger m => T.Text -> m ()
logInfoCyan = logLine . P.color P.Cyan . (<>) "[Info] "

logInfoSuccess :: MonadLogger m => T.Text -> m ()
logInfoSuccess = logLine . P.color P.Green . (<>) "[Info] "

logWarn :: MonadLogger m => T.Text -> m ()
logWarn = logLine . P.color P.Magenta . (<>) "[Warn] "

logError :: MonadLogger m => T.Text -> m ()
logError = logLine . P.color P.Red . (<>) "[Error] "
