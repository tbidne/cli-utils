module Common.Logging
  ( logError,
    logDebug,
    logInfo,
    logInfoBlue,
    logInfoSuccess,
    logWarn,
  )
where

import qualified Control.Monad.Logger as L
import qualified Data.Text as T
import qualified System.Console.Pretty as P

logDebug :: L.MonadLogger m => T.Text -> m ()
logDebug = L.logDebugN . P.color P.Blue

logInfo :: L.MonadLogger m => T.Text -> m ()
logInfo = L.logInfoN

logInfoBlue :: L.MonadLogger m => T.Text -> m ()
logInfoBlue = L.logInfoN . P.color P.Blue

logInfoSuccess :: L.MonadLogger m => T.Text -> m ()
logInfoSuccess = L.logInfoN . P.color P.Green

logWarn :: L.MonadLogger m => T.Text -> m ()
logWarn = L.logWarnN . P.color P.Magenta

logError :: L.MonadLogger m => T.Text -> m ()
logError = L.logErrorN . P.color P.Red
