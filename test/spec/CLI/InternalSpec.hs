{-# LANGUAGE OverloadedStrings #-}

module CLI.InternalSpec
  ( spec,
  )
where

import CLI.Internal
import qualified Data.Map as M
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "CLI.InternalSpec" $ do
    it "Should retrieve flat commands" $ do
      translateCommands mkMap ["cmd1"] `shouldBe` ["one"]
      translateCommands mkMap ["cmd2"] `shouldBe` ["two two"]
      translateCommands mkMap ["cmd3"] `shouldBe` ["three"]
    it "Should recursively search as far as possible" $ do
      translateCommands mkMap ["cmd5"] `shouldBe` ["one"]
    it "Should handle multiple commands" $ do
      translateCommands mkMap ["all,other"] `shouldBe` ["one", "two two", "three", "other"]

mkMap :: M.Map T.Text T.Text
mkMap =
  M.insert "cmd1" "one" $
  M.insert "cmd2" "two two" $
  M.insert "cmd3" "three" $
  M.insert "cmd4" "cmd1" $
  M.insert "cmd5" "cmd4" $
    M.insert "all" "cmd1,cmd2,cmd3" M.empty
