{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.MockSpec where

import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import Git.FastForward.Core.MockUpdateBranches
import Git.FastForward.Core.UpdateBranches
import Git.FastForward.Types.Env
import Git.FastForward.Types.MergeType
import Output
import Test.Hspec

spec :: Spec
spec = do
  describe "MockUpdateBranches Integration tests" $ do
    it "Mock run should process branches correctly" $ do
      let (Output res _) = runMock
      res `shouldSatisfy` verifyOutput

runMock :: Output ()
runMock = runReaderT (runMockUpdateBranchesT runUpdateBranches) env

env :: Env
env = Env Nothing Upstream

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
      "\"Checked out current\""
    ]
