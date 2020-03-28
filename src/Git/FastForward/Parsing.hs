{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Parsing
  ( parseArgs,
  )
where

import Common.Parsing
import qualified Data.Text as T
import Git.FastForward.Types.Env
import Git.FastForward.Types.MergeType
import Git.Types.GitTypes

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
    updater Env{mergeType} path = Env path mergeType

mergeTypeParser :: AnyParser Env
mergeTypeParser = AnyParser $ PrefixParser ("--merge-type=", parser, updater)
  where
    parser "" = Nothing
    parser "upstream" = Just Upstream
    parser "master" = Just Master
    parser o = Just $ Other $ Name $ T.pack o
    updater Env{path} = Env path

mergeUpstreamFlagParser :: AnyParser Env
mergeUpstreamFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-u" = Just Upstream
    parser _ = Nothing
    updater Env{path} = Env path

mergeMasterFlagParser :: AnyParser Env
mergeMasterFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-m" = Just Master
    parser _ = Nothing
    updater Env{path} = Env path

help :: String
help =
  "\nUsage: git-utils fastforward [OPTIONS]\n\n"
    <> "Fast-forwards all local branches with --ff-only.\n\nOptions:\n"
    <> "  --path=<string>\t\tDirectory path, defaults to current directory.\n\n"
    <> "  -u, --merge-type=upstream\tMerges each branches' upstream via @{u}. This is the default.\n\n"
    <> "  -m, --merge-type=master\tMerges origin/master into each branch.\n\n"
    <> "  --merge-type=<string>\t\tMerges branch given by <string> into each branch."
