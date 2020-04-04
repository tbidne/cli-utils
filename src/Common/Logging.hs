module Common.Logging
  ( logError,
    logDebug,
    logInfo,
    logInfoPretty,
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

logInfoPretty :: L.MonadLogger m => T.Text -> m ()
logInfoPretty = L.logInfoN . P.color P.Cyan

logWarn :: L.MonadLogger m => T.Text -> m ()
logWarn = L.logWarnN . P.color P.Magenta

logError :: L.MonadLogger m => T.Text -> m ()
logError = L.logErrorN . P.color P.Red
