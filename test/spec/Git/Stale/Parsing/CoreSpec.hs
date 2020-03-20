{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Git.Stale.Parsing.CoreSpec where

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Git.Stale.Parsing.Arbitraries
import Git.Stale.Parsing.Core
import qualified System.IO as IO
import Test.Hspec
import Test.Hspec.QuickCheck
import Git.Stale.Types.Env
import Git.Stale.Types.Nat

spec :: Spec
spec = do
  describe "Parsing tests" $ do
    it "Empty args uses defaults" $ do
      parseArgs mkDay [] `shouldSatisfy` verifyDefaults mkDay
    prop "Correctly parses valid args" parsesArgs
    prop "Invalid limit does not parse" invalidLimitDies
    prop "Invalid branch type does not parse" invalidBranchTypeDies
    prop "Invalid args do not parse" invalidArgsDies

verifyDefaults :: Cal.Day -> Either String Env -> Bool
verifyDefaults day =
  \case
    Left _ -> False
    Right Env {grepStr, path, limit, branchType, today} ->
      grepStr == Nothing
        && path == Just "/share"
        && Just limit == mkNat 30
        && branchType == Remote
        && today == day

parsesArgs :: Cal.Day -> ValidArgs -> Bool
parsesArgs d (ValidArgs g p l b _ _ args) =
  case parseArgs d args of
    Left _ -> False
    Right env ->
      verifyGrep g (grepStr env)
        && verifyPath p (path env)
        && verifyLimit l (limit env)
        && verifyBranchType b (branchType env)
        && d == today env

invalidLimitDies :: InvalidLimit -> Bool
invalidLimitDies (InvalidLimit s) =
  case parseArgs mkDay [s] of
    Left _ -> True
    Right _ -> False

invalidBranchTypeDies :: InvalidBranchType -> Bool
invalidBranchTypeDies (InvalidBranchType s) =
  case parseArgs mkDay [s] of
    Left _ -> True
    Right _ -> False

invalidArgsDies :: InvalidArgs -> Bool
invalidArgsDies (InvalidArgs args) =
  case parseArgs mkDay args of
    Left _ -> True
    Right _ -> False

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
