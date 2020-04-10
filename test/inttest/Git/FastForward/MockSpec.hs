{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.MockSpec
  ( spec,
  )
where

import App
import Control.Monad.Reader (runReaderT)
import Git.FastForward.Core.MockUpdateBranches ()
import Git.FastForward.Core.MonadUpdateBranches
import Git.FastForward.Parsing
import Git.FastForward.Types.Env
import qualified Data.Text as T
import Output
import Test.Hspec

spec :: Spec
spec = do
  describe "MockUpdateBranches Integration tests" $ do
    it "Mock run should process branches correctly" $ do
      let (Output logs ()) = runMock
      logs `shouldSatisfy` verifyOutput

runMock :: Output ()
runMock = runReaderT (runAppT runUpdateBranches) mkEnv

args :: [String]
args =
  [ "--path=",
    "--merge=master",
    "--push=origin push1, remote push2"
  ]

mkEnv :: Env
mkEnv = case parseArgs args of
  Left err -> error $ "Failure parsing args in integration test: " <> err
  Right env -> env

verifyOutput :: [T.Text] -> Bool
verifyOutput =
  (==)
    [ logInfoStr "That's so fetch",
      logInfoStr "",
      logInfoBlueStr "UPDATE SUMMARY",
      logInfoBlueStr "--------------",
      logInfoSuccessStr "Successes: [\"success2\",\"success1\"]",
      logInfoStr "No Change: [\"noChange2\",\"noChange1\"]",
      logWarnStr "Failures: [\"failure2\",\"failure1\"]\n",
      logInfoStr "",
      logInfoBlueStr "PUSH SUMMARY",
      logInfoBlueStr "------------",
      logInfoSuccessStr "Successes: [\"remote push2\",\"origin push1\"]",
      logInfoStr "No Change: []",
      logWarnStr "Failures: []\n",
      logInfoStr "Checked out current"
    ]

logInfoBlueStr :: T.Text -> T.Text
logInfoBlueStr s = "\ESC[94m[Info] " <> s <> logEnd

logInfoStr :: T.Text -> T.Text
logInfoStr = (<>) "[Info] "

logInfoSuccessStr :: T.Text -> T.Text
logInfoSuccessStr s = "\ESC[92m[Info] " <> s <> logEnd

logWarnStr :: T.Text -> T.Text
logWarnStr s = "\ESC[95m[Warn] " <> s <> logEnd

logEnd :: T.Text
logEnd = "\ESC[0m"
