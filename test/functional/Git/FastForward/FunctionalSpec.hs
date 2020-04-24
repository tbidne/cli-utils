{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.FunctionalSpec
  ( spec,
  )
where

import App
import qualified Control.Monad.Reader as R
import qualified Data.Text as T
import Git.FastForward.Core.MonadUpdateBranches
import Git.FastForward.Parsing
import Git.FastForward.Types.Env
import qualified System.IO as IO
import qualified System.IO.Silently as Shh
import qualified System.Process as P
import Test.Hspec

spec :: Spec
spec = afterAll_ tearDown $ beforeAll_ setup $ do
  describe "Git.FastForward.FunctionalSpec" $ do
    it itMessage $ do
      env <- mkEnv
      output <- Shh.capture_ (runTest env)
      T.lines (T.pack output) `shouldSatisfy` allTrue . foldMap sToVerifier

itMessage :: String
itMessage =
  "Should be:\n"
    <> "      Successes:  [can-ff-one, can-ff-two]\n"
    <> "      No Changes: [master]\n"
    <> "      Failures:   [cannot-ff]"

allTrue :: Verifier -> Bool
allTrue (Verifier True True True) = True
allTrue _ = False

sToVerifier :: T.Text -> Verifier
sToVerifier s
  | T.isPrefixOf expectedSuccesses s = mempty {vSuccesses = True}
  | T.isPrefixOf expectedNoChanges s = mempty {vNoChanges = True}
  | T.isPrefixOf expectedFailures s = mempty {vFailures = True}
  | otherwise = mempty

runTest :: Env -> IO ()
runTest env = R.runReaderT (runAppT runUpdateBranches) env

mkEnv :: IO Env
mkEnv = do
  let args =
        [ "--path=./scripts/testing/git-ff",
          "--merge=master",
          "--no-fetch"
        ]
  case parseArgs args of
    Right env -> pure env
    Left e -> error ("Error parsing args: " <> show e)

setup :: IO ()
setup =
  let proc = (P.shell "./setup_git_ff.sh") {P.cwd = (Just "./scripts/testing/")}
   in Shh.hSilence [IO.stderr] $ P.readCreateProcess proc "" *> pure ()

tearDown :: IO ()
tearDown =
  let proc = (P.shell "./teardown_git_ff.sh") {P.cwd = (Just "./scripts/testing/")}
   in P.readCreateProcess proc "" *> pure ()

data Verifier
  = Verifier
      { vSuccesses :: Bool,
        vNoChanges :: Bool,
        vFailures :: Bool
      }

instance Semigroup Verifier where
  (Verifier a b c) <> (Verifier a' b' c') =
    Verifier
      (a || a')
      (b || b')
      (c || c')

instance Monoid Verifier where
  mempty = Verifier False False False

expectedSuccesses :: T.Text
expectedSuccesses =
  infoSuccessPrefix
    <> "Successes: [\"can-ff-one\",\"can-ff-two\"]"

expectedNoChanges :: T.Text
expectedNoChanges = infoPrefix <> "No Change: [\"master\"]"

expectedFailures :: T.Text
expectedFailures = warnPrefix <> "Failures: [\"cannot-ff\"]"

infoPrefix :: T.Text
infoPrefix = "[Info] "

infoSuccessPrefix :: T.Text
infoSuccessPrefix = "\ESC[92m[Info] "

warnPrefix :: T.Text
warnPrefix = "\ESC[95m[Warn] "
