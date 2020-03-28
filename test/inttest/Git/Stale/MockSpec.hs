{-# LANGUAGE OverloadedStrings #-}

module Git.Stale.MockSpec where

import Control.Monad.Reader (runReaderT)
import qualified Data.Maybe as M
import qualified Data.Text as Txt
import qualified Data.Time.Calendar as Cal
import Git.Stale.Core.FindBranches
import Git.Stale.Core.MockFindBranches
import Git.Stale.Types.Env
import Git.Stale.Types.Nat
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

runMock :: Txt.Text -> Output ()
runMock t = runReaderT (runMockFindBranchesT runFindBranches) (envWithGrep t)

envWithGrep :: Txt.Text -> Env
envWithGrep "" = Env Nothing (Just "/share") unsafeNat Remote "origin/" "origin/master" mkDay
envWithGrep s = Env (Just s) (Just "/share") unsafeNat Remote "origin/" "origin/master" mkDay

mkDay :: Cal.Day
mkDay = Cal.fromGregorian 2017 7 27

unsafeNat :: Nat
unsafeNat = M.fromJust $ mkNat 30
