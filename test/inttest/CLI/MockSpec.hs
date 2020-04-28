{-# LANGUAGE OverloadedStrings #-}

module CLI.MockSpec
  ( spec,
  )
where

import App
import CLI.MockCLI ()
import CLI.MonadCLI
import CLI.Parsing.Internal
import CLI.Types.Env
import Common.Types.NonNegative
import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import Output
import Test.Hspec

spec :: Spec
spec = do
  describe "CLI.MockSpec" $ do
    it "Mock should output commands" $ do
      let (Output res _) = runMock
      res `shouldBe` ["command 1", "command 2", "do a", "do b", " do c"]

runMock :: Output ()
runMock = runReaderT (runAppT runCLI) mkEnv

mockCommands :: [T.Text]
mockCommands =
  [ "command 1",
    "command 2",
    "several"
  ]

mapStr :: String
mapStr =
  "a=do a\n"
    <> "b=do b\n"
    <> "c= do c\n"
    <> "#some comment\n"
    <> "several=a,b,c\n"
    <> "extra=not happening"

timeoutArg :: Maybe (NonNegative Int)
timeoutArg = toNonNegative 5

mkEnv :: Env
mkEnv = case mapStrToEnv mockCommands timeoutArg mapStr of
  Left err -> error $ "Failure parsing args in int test: " <> show err
  Right env -> env
