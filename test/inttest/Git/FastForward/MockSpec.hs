{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.MockSpec where

import Control.Monad.Identity
import Control.Monad.Logger
import Control.Monad.Reader (runReaderT)
import Git.FastForward.Core.MockUpdateBranches
import Git.FastForward.Core.MonadUpdateBranches
import Git.FastForward.Parsing
import Git.FastForward.Types.Env
import Test.Hspec

spec :: Spec
spec = do
  describe "MockUpdateBranches Integration tests" $ do
    it "Mock run should process branches correctly" $ do
      let (Identity (_, res)) = runMock
      lineToLog <$> res `shouldSatisfy` verifyOutput

runMock :: Identity ((), [LogLine])
runMock = runWriterLoggingT (runReaderT (runMockUpdateT runUpdateBranches) mkEnv)

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

lineToLog :: LogLine -> LogStr
lineToLog (_, _, _, x) = x

verifyOutput :: [LogStr] -> Bool
verifyOutput =
  (==)
    [ "Fetching",
      "",
      logInfoBlueStr "UPDATE SUMMARY",
      logInfoBlueStr "--------------",
      logInfoSuccessStr "Successes: [\"success2\",\"success1\"]",
      logInfoStr "No Change: [\"noChange2\",\"noChange1\"]",
      logWarnStr "Failures: [\"failure2\",\"failure1\"]\n",
      "",
      logInfoBlueStr "PUSH SUMMARY",
      logInfoBlueStr "------------",
      logInfoSuccessStr "Successes: [\"remote push2\",\"origin push1\"]",
      logInfoStr "No Change: []",
      logWarnStr "Failures: []\n",
      "Checked out current"
    ]

logInfoBlueStr :: LogStr -> LogStr
logInfoBlueStr s = "\ESC[94m" <> s <> logEnd

logInfoStr :: LogStr -> LogStr
logInfoStr = id

logInfoSuccessStr :: LogStr -> LogStr
logInfoSuccessStr s = "\ESC[92m" <> s <> logEnd

logWarnStr :: LogStr -> LogStr
logWarnStr s = "\ESC[95m" <> s <> logEnd

logEnd :: LogStr
logEnd = "\ESC[0m"
