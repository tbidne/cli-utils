module Git.Stale.MockSpec where

import Control.Monad.Reader (runReaderT)
import qualified Data.Time.Calendar as Cal
import Git.Stale.Core.FindBranches
import Git.Stale.Core.MockFindBranches
import Git.Stale.Types.Env
import Git.Stale.Parsing
import Output
import Test.Hspec

spec :: Spec
spec = do
  describe "MockFindBranches Integration tests" $ do
    it "Mock run with grep `branch` should return 5 results" $ do
      let (Output res _) = runMock "branch"
      length res `shouldBe` 5
    it "Mock run with grep `other` should return 3 results" $ do
      let (Output res _) = runMock "other"
      length res `shouldBe` 3
    it "Mock run with blank grep should return 8 results" $ do
      let (Output res _) = runMock ""
      length res `shouldBe` 8
    it "Mock run with wrong grep should return 0 results" $ do
      let (Output res _) = runMock "nope"
      length res `shouldBe` 0

runMock :: String -> Output ()
runMock t = runReaderT (runMockFindBranchesT runFindBranches) (envWithGrep t)

args :: String -> [String]
args s =
  [
    "--grep=" <> s,
    "--path=/share",
    "--limit=30",
    "--branch-type=remote",
    "--remote=origin/",
    "--master=origin/master"
  ]

envWithGrep :: String -> Env
envWithGrep s = case parseArgs mkDay (args s) of
  Left err -> error $ "Failure parsing args in int test: " <> err
  Right env -> env

mkDay :: Cal.Day
mkDay = Cal.fromGregorian 2017 7 27