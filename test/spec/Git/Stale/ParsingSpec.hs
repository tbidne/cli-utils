{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Git.Stale.ParsingSpec
  ( spec,
  )
where

import Common.Parsing.Core
import Common.Utils
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Git.Stale.Arbitraries
import Git.Stale.Parsing
import Git.Stale.Types.Env
import Common.Types.NonNegative
import qualified System.IO as IO
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Git.Stale.ParsingSpec" $ do
    it "Empty args uses defaults" $ do
      parseArgs mkDay [] `shouldSatisfy` verifyDefaults mkDay
    prop "Correctly parses valid args" parsesArgs
    prop "Invalid limit does not parse" invalidLimitDies
    prop "Invalid branch type does not parse" invalidBranchTypeDies
    prop "Invalid args do not parse" invalidArgsDies

verifyDefaults :: Cal.Day -> Either ParseErr Env -> Bool
verifyDefaults day =
  \case
    Left _ -> False
    Right Env {grepStr, path, limit, branchType, today} ->
      grepStr == Nothing
        && path == Nothing
        && Just limit == toNonNegative (30 :: Int)
        && branchType == Remote
        && today == day

parsesArgs :: Cal.Day -> ValidArgs -> Bool
parsesArgs d (ValidArgs g p l b _ _ args) =
  case parseArgs d args of
    Left _ -> False
    Right Env {grepStr, path, limit, branchType, today} ->
      verifyGrep g grepStr
        && verifyPath p path
        && verifyLimit l limit
        && verifyBranchType b branchType
        && d == today

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
verifyGrep (matchAndStrip "--grep=" -> Just s) =
  \case
    Nothing -> s == ""
    Just t -> s == T.unpack t
verifyGrep _ = error "Bad grep passed to test"

verifyPath :: String -> Maybe IO.FilePath -> Bool
verifyPath (matchAndStrip "--path=" -> Just s) =
  \case
    Nothing -> s == ""
    Just t -> s == t
verifyPath _ = error "Bad path passed to test"

verifyLimit :: String -> NonNegative Int -> Bool
verifyLimit (matchAndStrip "--limit=" -> Just s) = ((s ==) . show . getNonNegative)
verifyLimit _ = error "Bad limit passed to test"

verifyBranchType :: String -> BranchType -> Bool
verifyBranchType "--branch-type=all" All = True
verifyBranchType "-a" All = True
verifyBranchType "--branch-type=remote" Remote = True
verifyBranchType "-r" Remote = True
verifyBranchType "--branch-type=local" Local = True
verifyBranchType "-l" Local = True
verifyBranchType _ _ = False

mkDay :: Cal.Day
mkDay = Cal.fromGregorian 2017 7 27
