{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.MonadLogger
  ( MonadLogger (..),
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

class Monad m => MonadLogger m where
  logTxt :: T.Text -> m ()

instance MonadLogger IO where
  logTxt = putStrLn . T.unpack

instance MonadLogger m => MonadLogger (AppT env m) where
  logTxt = MTL.lift . logTxt

logDebug :: MonadLogger m => T.Text -> m ()
logDebug = logTxt . (<>) "[Debug] "

logInfo :: MonadLogger m => T.Text -> m ()
logInfo = logTxt . (<>) "[Info] "

logInfoBlue :: MonadLogger m => T.Text -> m ()
logInfoBlue = logTxt . P.color P.Blue . (<>) "[Info] "

logInfoCyan :: MonadLogger m => T.Text -> m ()
logInfoCyan = logTxt . P.color P.Cyan . (<>) "[Info] "

logInfoSuccess :: MonadLogger m => T.Text -> m ()
logInfoSuccess = logTxt . P.color P.Green . (<>) "[Info] "

logWarn :: MonadLogger m => T.Text -> m ()
logWarn = logTxt . P.color P.Magenta . (<>) "[Warn] "

logError :: MonadLogger m => T.Text -> m ()
logError = logTxt . P.color P.Red . (<>) "[Error] "
