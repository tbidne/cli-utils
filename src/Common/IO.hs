{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Common.Utils
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports common `IO` functions.
module Common.IO
  ( sh,
    sh_,
    trySh,
    tryShAndReturnStdErr,
    tryShExitCode,
    tryTimeSh,
  )
where

import Common.Types.NonNegative
import Common.Utils
import qualified Control.Exception as Ex
import Data.Functor (($>))
import qualified Data.Text as T
import qualified System.Clock as C
import qualified System.Exit as Exit
import qualified System.Process as P

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

-- | Version of 'sh' that returns ('Exit.ExitCode', stdout, stderr)
shExitCode :: T.Text -> Maybe FilePath -> IO (Exit.ExitCode, String, String)
shExitCode cmd path = P.readCreateProcessWithExitCode proc ""
  where
    proc = (P.shell (T.unpack cmd)) {P.cwd = path}

-- | Attempts to return the result of running a shell command given by
-- 'T.Text' on 'FilePath'.
trySh :: T.Text -> Maybe FilePath -> IO (Either Ex.SomeException T.Text)
trySh cmd path = Ex.try (sh cmd path)

-- | This is an odd function; it returns stderr on both success and failure.
-- Why is this useful? There are some shell commands that set the return code
-- to 0 (success) but also return information we care about as stderr.
-- Performing git push on a branch that is already up-to-date is an example
-- that "succeeds", but the output we care about ("Everything up-to-date") is
-- inexplicably sent to stderr instead of stdout. This function is meant for
-- these situations, where we still want the usual `trySh_` semantics, but want
-- to paradoxically return stderr on success.
tryShAndReturnStdErr :: T.Text -> Maybe FilePath -> IO (Either T.Text T.Text)
tryShAndReturnStdErr cmd path = do
  (code, _, err) <- shExitCode cmd path
  pure $ case code of
    Exit.ExitSuccess -> Right $ T.strip (T.pack err)
    Exit.ExitFailure _ ->
      Left $
        "Error running `"
          <> cmd
          <> "`: "
          <> T.strip (T.pack err)

-- | Returns 'Left' stderr if there is a failure, 'Right' stdout otherwise.
tryShExitCode :: T.Text -> Maybe FilePath -> IO (Either T.Text T.Text)
tryShExitCode cmd path = do
  (code, out, err) <- shExitCode cmd path
  pure $ case code of
    Exit.ExitSuccess -> Right $ T.strip (T.pack out)
    Exit.ExitFailure _ ->
      Left $
        "Error running `"
          <> cmd
          <> "`: "
          <> T.strip (T.pack err)

-- | Version of 'tryShExitCode' that also returns (t, stdout/stderr), where
-- /t/ is the time the command took in seconds.
tryTimeSh :: T.Text -> Maybe FilePath -> IO (Either (NonNegative, T.Text) (NonNegative, T.Text))
tryTimeSh cmd path = do
  start <- C.getTime C.Monotonic
  res <- tryShExitCode cmd path
  end <- C.getTime C.Monotonic
  let diff = diffTime start end
  pure $ monoBimap (diff,) res
