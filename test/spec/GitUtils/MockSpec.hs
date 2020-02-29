{-# LANGUAGE OverloadedStrings #-}

module GitUtils.MockSpec where

import Control.Monad.Reader (runReaderT)
import qualified Data.Text as Txt
import Test.Hspec

import App
import GitUtils.Core
import Types.Env

import GitUtils.MockUtils

spec :: Spec
spec = do
  describe "MockUtils tests" $ do
    it "Mock run with grep `branch` should return 5 results" $ do
      let (MockUtils res _) = runMock "branch"
      length res `shouldBe` 5
    
    it "Mock run with grep `other` should return 3 results" $ do
      let (MockUtils res _) = runMock "other"
      length res `shouldBe` 3
    
    it "Mock run with blank grep should return 8 results" $ do
      let (MockUtils res _) = runMock ""
      length res `shouldBe` 8
    
    it "Mock run with wrong grep should return 0 results" $ do
      let (MockUtils res _) = runMock "nope"
      length res `shouldBe` 0

runMock :: Txt.Text -> MockUtils ()
runMock t = do
  runReaderT (runAppT runWithReader) (envWithGrep t)

envWithGrep :: Txt.Text -> Env
envWithGrep "" = Env Nothing undefined undefined undefined
envWithGrep s  = Env (Just s) undefined undefined undefined