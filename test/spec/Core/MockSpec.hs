{-# LANGUAGE OverloadedStrings #-}

module Core.MockSpec where

import Control.Monad.Reader (runReaderT)
import Core.MockUtils
import Core.MonadGit
import qualified Data.Text as Txt
import Test.Hspec
import Types.Env

spec :: Spec
spec = do
  describe "MockUtils tests" $ do
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
runMock t = do
  runReaderT (runMockUtilsT runGitUtils) (envWithGrep t)

envWithGrep :: Txt.Text -> Env
envWithGrep "" = Env Nothing undefined undefined undefined
envWithGrep s = Env (Just s) undefined undefined undefined
