{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Git.Stale.FunctionalSpec where

import App
import Common.Utils
import qualified Control.Monad.Logger as L
import qualified Control.Monad.Reader as R
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as Clock
import Git.Stale.Core.MonadFindBranches
import Git.Stale.Parsing
import Git.Stale.Types.Env
import qualified System.IO as IO
import qualified System.IO.Silently as Shh
import qualified System.Process as P
import Test.Hspec

spec :: Spec
spec = afterAll_ tearDown $ beforeAll_ setup $ do
  describe "IO Functional tests with real git repo" $ do
    it "Should have 0 Errors, 20 Merged, and 14 Unmerged" $ do
      env <- mkEnv
      output <- Shh.capture_ (runTest env)
      lines output `shouldSatisfy` verifyOutput

runTest :: Env -> IO ()
runTest env = L.runStdoutLoggingT (R.runReaderT (runAppT runFindBranches) env)

mkEnv :: IO Env
mkEnv = do
  d <- currDay
  let args =
        [ "--grep=1",
          "--path=./scripts/testing/test-git",
          "--branch-type=local",
          "--master=master",
          "--limit=0"
        ]
  case parseArgs d args of
    Right env -> return env
    Left e -> error ("Error parsing args: " <> show e)

setup :: IO ()
setup =
  let proc = (P.shell "./setup_git_repo.sh") {P.cwd = (Just "./scripts/testing/")}
   in Shh.hSilence [IO.stderr] $ P.readCreateProcess proc "" *> pure ()

tearDown :: IO ()
tearDown =
  let proc = (P.shell "./teardown.sh") {P.cwd = (Just "./scripts/testing/")}
   in P.readCreateProcess proc "" *> pure ()

currDay :: IO Cal.Day
currDay = fmap Clock.utctDay Clock.getCurrentTime

verifyOutput :: [String] -> Bool
verifyOutput = allTrue . toVerifier

newtype Verifier = Verifier (Bool, Bool, Bool)

instance Semigroup Verifier where
  (Verifier (x, y, z)) <> (Verifier (x', y', z')) = Verifier ((x || x'), (y || y'), (z || z'))

instance Monoid Verifier where
  mempty = Verifier (False, False, False)

allTrue :: Verifier -> Bool
allTrue (Verifier (True, True, True)) = True
allTrue _ = False

toVerifier :: [String] -> Verifier
toVerifier = foldr ((<>) . f) mempty
  where
    f (matchAndStrip "[Warn] \ESC[95mERRORS: " -> Just res) =
      Verifier ((res == "0\ESC[0m"), False, False)
    f (matchAndStrip "[Info] \ESC[92mMERGED: " -> Just res) =
      Verifier (False, (res == "20\ESC[0m"), False)
    f (matchAndStrip "[Info] \ESC[96mUNMERGED: " -> Just res) =
      Verifier (False, False, (res == "14\ESC[0m"))
    f _ = mempty
