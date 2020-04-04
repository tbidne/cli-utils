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
      infoPrettyColor
        <> "\n\nUPDATE SUMMARY\n--------------\n"
        <> "Successes: [\"success2\",\"success1\"]\n"
        <> "No Changes: [\"noChange2\",\"noChange1\"]\n"
        <> "Failures: [\"failure2\",\"failure1\"]\n"
        <> endColor,
      infoPrettyColor
        <> "\n\nPUSH SUMMARY\n-----------\n"
        <> "Successes: [\"remote push2\",\"origin push1\"]\n"
        <> "No Changes: []\n"
        <> "Failures: []\n"
        <> endColor,
      "Checked out current"
    ]

infoPrettyColor :: LogStr
infoPrettyColor = "\ESC[96m"

endColor :: LogStr
endColor = "\ESC[0m"
