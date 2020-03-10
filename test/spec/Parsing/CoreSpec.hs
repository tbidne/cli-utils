{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Parsing.CoreSpec where

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Parsing.Arbitraries
import Parsing.Core
import qualified System.IO as IO
import Test.Hspec
import Test.Hspec.QuickCheck
import Types.Env

spec :: Spec
spec = do
  describe "Parsing tests" $ do
    it "Empty args uses defaults" $ do
      parseArgs mkDay [] `shouldSatisfy` verifyDefaults mkDay
    prop "Correctly parses valid args" parsesArgs

verifyDefaults :: Cal.Day -> Either String Env -> Bool
verifyDefaults day =
  \case
    Left _ -> False
    Right env ->
      grepStr env == Nothing
        && path env == Just "/share"
        && Just (limit env) == mkNat 30
        && branchType env == Remote
        && today env == day

parsesArgs :: Cal.Day -> ValidArgs -> Bool
parsesArgs d (ValidArgs g p l b args) =
  case parseArgs d args of
    Left _ -> False
    Right env ->
      verifyGrep g (grepStr env)
        && verifyPath p (path env)
        && verifyLimit l (limit env)
        && verifyBranchType b (branchType env)
        && d == today env

verifyGrep :: String -> Maybe T.Text -> Bool
verifyGrep (startsWith "--grep=" -> Just s) =
  \case
    Nothing -> s == ""
    Just t -> s == T.unpack t
verifyGrep _ = error "Bad grep passed to test"

verifyPath :: String -> Maybe IO.FilePath -> Bool
verifyPath (startsWith "--path=" -> Just s) =
  \case
    Nothing -> s == ""
    Just t -> s == t
verifyPath _ = error "Bad path passed to test"

verifyLimit :: String -> Nat -> Bool
verifyLimit (startsWith "--limit=" -> Just s) = ((s ==) . show . unNat)
verifyLimit _ = error "Bad limit passed to test"

verifyBranchType :: String -> BranchType -> Bool
verifyBranchType (startsWith "--branchType=" -> Just s) =
  \case
    All -> s `elem` ["a", "all"]
    Remote -> s `elem` ["r", "remote"]
    Local -> s `elem` ["l", "local"]
verifyBranchType _ = error "Bad branch type passed to test"

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith [] ys = Just ys
startsWith _ [] = Nothing
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = Nothing

mkDay :: Cal.Day
mkDay = Cal.fromGregorian 2017 7 27
