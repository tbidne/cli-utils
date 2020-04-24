{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Git.FastForward.ParsingSpec
  ( spec,
  )
where

import Common.Parsing.Core
import Common.Utils
import qualified Data.Text as T
import Git.FastForward.Arbitraries
import Git.FastForward.Parsing
import Git.FastForward.Types.Env
import Git.FastForward.Types.MergeType
import Git.Types.GitTypes
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "FastForward parsing tests" $ do
    it "Empty args uses defaults" $ do
      parseArgs [] `shouldSatisfy` verifyDefaults
    prop "Correctly parses valid args" parsesArgs
    prop "Invalid args do not parse" invalidArgsDies

verifyDefaults :: Either ParseErr Env -> Bool
verifyDefaults =
  \case
    Left _ -> False
    Right Env {path, mergeType} ->
      path == Nothing
        && mergeType == Upstream

parsesArgs :: ValidArgs -> Bool
parsesArgs ValidArgs {validPath, validMergeType, validDoFetch, order = args} =
  case parseArgs args of
    Left _ -> False
    Right Env {path, mergeType, doFetch} ->
      verifyPath validPath path
        && verifyMergeType validMergeType mergeType
        && verifyDoFetch validDoFetch doFetch

verifyPath :: String -> Maybe FilePath -> Bool
verifyPath "--path=" Nothing = True
verifyPath (matchAndStrip "--path=" -> Just s) (Just s') = s == s'
verifyPath _ _ = False

verifyMergeType :: String -> MergeType -> Bool
verifyMergeType "--merge=upstream" Upstream = True
verifyMergeType "-u" Upstream = True
verifyMergeType "-m" Master = True
verifyMergeType (matchAndStrip "--merge=" -> Just s) (Other (Name n)) = (T.pack s) == n
verifyMergeType _ _ = False

verifyDoFetch :: [String] -> Bool -> Bool
verifyDoFetch ["--no-fetch"] False = True
verifyDoFetch [] True = True
verifyDoFetch _ _ = False

invalidArgsDies :: InvalidArgs -> Bool
invalidArgsDies (InvalidArgs args) =
  case parseArgs args of
    Left _ -> True
    Right _ -> False
