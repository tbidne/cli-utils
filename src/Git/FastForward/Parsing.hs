{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.FastForward.Parsing
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Handles parsing of 'String' args into 'Env'.
module Git.FastForward.Parsing
  ( parseArgs,
  )
where

import Common.Parsing
import qualified Data.Text as T
import Git.FastForward.Types.Env
import Git.FastForward.Types.MergeType
import Git.Types.GitTypes

-- | Maps parsed [`String`] args into `Right` `Env`, returning
-- any errors as `Left` `String`. All arguments are optional
-- (i.e. an empty list is valid), but if any are provided then they must
-- be valid or an error will be returned. Valid arguments are:
--
-- @
--   --path=\<string>\
--       Path to the git directory. Any `String` is fine, defaults
--       to the empty string (current directory).
--
--   -u, --merge-type=upstream
--       Merges upstream via @{u} into each local branch. This is the default.
--
--   -m, --branch-type=master
--       Merges origin/master into each local branch.
--
--   --branch-type=\<other\>
--       Merges \<other\> into each local branch.
--
--   -h, --help
--       Returns instructions as `Left` `String`.
-- @
parseArgs :: [String] -> Either String Env
parseArgs args =
  case parseAll allParsers args defaultEnv of
    Left Help -> Left help
    Left (Err arg) -> Left $ "Could not parse `" <> arg <> "`. Try --help."
    Right env -> Right env

defaultEnv :: Env
defaultEnv =
  Env
    Nothing
    Upstream

allParsers :: [AnyParser Env]
allParsers =
  [ pathParser,
    mergeTypeParser,
    mergeUpstreamFlagParser,
    mergeMasterFlagParser
  ]

pathParser :: AnyParser Env
pathParser = AnyParser $ PrefixParser ("--path=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just s
    updater env p = env {path = p}

mergeTypeParser :: AnyParser Env
mergeTypeParser = AnyParser $ PrefixParser ("--merge-type=", parser, updater)
  where
    parser "" = Nothing
    parser "upstream" = Just Upstream
    parser "master" = Just Master
    parser o = Just $ Other $ Name $ T.pack o
    updater env m = env {mergeType = m}

mergeUpstreamFlagParser :: AnyParser Env
mergeUpstreamFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-u" = Just Upstream
    parser _ = Nothing
    updater env m = env {mergeType = m}

mergeMasterFlagParser :: AnyParser Env
mergeMasterFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-m" = Just Master
    parser _ = Nothing
    updater env m = env {mergeType = m}

help :: String
help =
  "\nUsage: git-utils fastforward [OPTIONS]\n\n"
    <> "Fast-forwards all local branches with --ff-only.\n\nOptions:\n"
    <> "  --path=<string>\t\tDirectory path, defaults to current directory.\n\n"
    <> "  -u, --merge-type=upstream\tMerges each branches' upstream via @{u}. This is the default.\n\n"
    <> "  -m, --merge-type=master\tMerges origin/master into each branch.\n\n"
    <> "  --merge-type=<string>\t\tMerges branch given by <string> into each branch."
