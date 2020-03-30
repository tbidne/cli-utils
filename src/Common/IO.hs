-- |
-- Module      : Common.Utils
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports common `IO` functions.
module Common.IO
  ( sh,
    sh_,
    shCaptureErr_,
    trySh,
    trySh_,
    tryShCaptureErr_,
  )
where

import qualified Control.Exception as Ex
import Data.Functor (($>))
import qualified Data.Text as T
import qualified System.Process as P
import qualified System.IO.Silently as Shh
import qualified System.IO as IO

-- | Returns the result of running a shell command given by
-- 'T.Text' on 'FilePath'.
sh :: T.Text -> Maybe FilePath -> IO T.Text
sh cmd fp = T.pack <$> P.readCreateProcess proc ""
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

-- | Version of 'sh' that ignores the return value.
sh_ :: T.Text -> Maybe FilePath -> IO ()
sh_ cmd fp = P.readCreateProcess proc "" $> ()
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = fp}

-- | Version of 'sh' that ignores the return value and returns stderr.
shCaptureErr_ :: T.Text -> Maybe FilePath -> IO T.Text
shCaptureErr_ cmd path = T.pack <$> (Shh.hCapture_ [IO.stderr] $ sh_ cmd path)

-- | Attempts to return the result of running a shell command given by
-- 'T.Text' on 'FilePath'.
trySh :: T.Text -> Maybe FilePath -> IO (Either Ex.SomeException T.Text)
trySh cmd path = Ex.try (sh cmd path)

-- | Version of 'trySh' that ignores the return value.
trySh_ :: String -> T.Text -> Maybe FilePath -> IO ()
trySh_ err cmd path = do
  res <- Ex.try (sh_ cmd path) :: IO (Either Ex.SomeException ())
  case res of
    Left ex -> do
      putStrLn $ err <> ": " <> show ex
      pure ()
    Right r -> pure r

-- | Version of 'trySh' that returns stderr.
tryShCaptureErr_ :: T.Text -> Maybe FilePath -> IO (Either Ex.SomeException T.Text)
tryShCaptureErr_ cmd path = Ex.try (shCaptureErr_ cmd path)