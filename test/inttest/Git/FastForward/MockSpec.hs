{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.MockSpec where

import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import Git.FastForward.Core.MockUpdateBranches
import Git.FastForward.Core.MonadUpdateBranches
import Git.FastForward.Parsing
import Git.FastForward.Types.Env
import Output
import Test.Hspec

spec :: Spec
spec = do
  describe "MockUpdateBranches Integration tests" $ do
    it "Mock run should process branches correctly" $ do
      let (Output res _) = runMock
      res `shouldSatisfy` verifyOutput

runMock :: Output ()
runMock = runReaderT (runMockUpdateT runUpdateBranches) mkEnv

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
    [ "\"Fetching\"",
      "Success (Name \"success1\")",
      "Success (Name \"success2\")",
      "NoChange (Name \"noChange1\")",
      "NoChange (Name \"noChange2\")",
      "Failure (Name \"failure1\")",
      "Failure (Name \"failure2\")",
      "Success (Name \"origin push1\")",
      "Success (Name \"remote push2\")",
      "\"Checked out current\""
    ]
