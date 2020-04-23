{-# LANGUAGE NamedFieldPuns #-}
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

import Common.Parsing.Core
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Git.FastForward.Types.Env
import Git.FastForward.Types.MergeType
import Git.Types.GitTypes

-- | Maps parsed [`String`] args into `Right` `Env`, returning
-- any errors as `Left` `ParseErr`. All arguments are optional
-- (i.e. an empty list is valid), but if any are provided then they must
-- be valid or an error will be returned. Valid arguments are:
--
-- @
--   --path=\<string>\
--       Path to the git directory. Any `String` is fine, defaults
--       to the empty string (current directory).
--
--   -u, --merge=upstream
--       Merges upstream via @{u} into each local branch. This is the default.
--
--   -m, --branch-type=master
--       Merges origin/master into each local branch.
--
--   --branch-type=\<other\>
--       Merges \<other\> into each local branch.
--
--   --push=<\list\>
--       List of branches to push to after we're done updating.
--       Each branch is formatted "remote_name branch_name",
--       and each "remote_name branch_name" is separated by a comma.
--       For instance, --push="origin dev, other temp".
--
--   -h, --help
--       Returns instructions as `Left` `String`.
-- @
parseArgs :: [String] -> Either ParseErr Env
parseArgs args =
  case parseAll allParsers args of
    ParseAnd (PFailure (Help _)) -> Left $ Help help
    ParseAnd (PFailure (Err arg)) ->
      Left $ Err $ "Could not parse `" <> arg <> "`. Try --help."
    ParseAnd (PSuccess acc) -> Right $ accToEnv acc

newtype AccMergeType = AccMergeType MergeType deriving (Show)

instance Semigroup AccMergeType where
  (AccMergeType Upstream) <> r = r
  l <> _ = l

instance Monoid AccMergeType where
  mempty = AccMergeType Upstream

data Acc
  = Acc
      { accPath :: Maybe FilePath,
        accMergeType :: AccMergeType,
        accPush :: [Name]
      }
  deriving (Show)

instance Semigroup Acc where
  Acc {accPath = a, accMergeType = m, accPush = n}
    <> Acc {accPath = a', accMergeType = m', accPush = n'} =
      Acc (a <|> a') (m <> m') (n <> n')

instance Monoid Acc where
  mempty = Acc mempty mempty mempty

accToEnv :: Acc -> Env
accToEnv Acc {accPath, accMergeType = (AccMergeType m), accPush} =
  Env {path = accPath, mergeType = m, push = accPush}

allParsers :: [AnyParser Acc]
allParsers =
  [ pathParser,
    mergeTypeParser,
    mergeFlagParser,
    pushBranchesParser
  ]

pathParser :: AnyParser Acc
pathParser = AnyParser $ PrefixParser ("--path=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just s
    updater acc p = acc {accPath = p}

mergeTypeParser :: AnyParser Acc
mergeTypeParser = AnyParser $ PrefixParser ("--merge=", parser, updater)
  where
    parser "" = Nothing
    parser "upstream" = Just $ AccMergeType Upstream
    parser "master" = Just $ AccMergeType Master
    parser o = Just $ AccMergeType $ Other $ Name $ T.pack o
    updater acc m = acc {accMergeType = m}

mergeFlagParser :: AnyParser Acc
mergeFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-u" = Just $ AccMergeType Upstream
    parser "-m" = Just $ AccMergeType Master
    parser _ = Nothing
    updater acc m = acc {accMergeType = m}

pushBranchesParser :: AnyParser Acc
pushBranchesParser = AnyParser $ PrefixParser ("--push=", parser, updater)
  where
    parser "" = Nothing
    parser s = Just $ Name . T.strip <$> T.splitOn "," (T.pack s)
    updater acc ps = acc {accPush = ps}

help :: String
help =
  "\nUsage: cli-utils fastforward [OPTIONS]\n\n"
    <> "Fast-forwards all local branches with --ff-only.\n\nOptions:\n"
    <> "  --path=<string>\tDirectory path, defaults to current directory.\n\n"
    <> "  -u, --merge=upstream\tMerges each branches' upstream via @{u}. This is the default.\n\n"
    <> "  -m, --merge=master\tMerges origin/master into each branch.\n\n"
    <> "  --merge=<string>\tMerges branch given by <string> into each branch.\n\n"
    <> "  --push=<list>\t\tList of branches to push to after we're done updating.\n"
    <> "\t\t\tEach branch is formatted \"remote_name branch_name\",\n"
    <> "\t\t\tand each \"remote_name branch_name\" is separated by a comma.\n"
    <> "\t\t\tFor instance, --push=\"origin dev, other temp\"."
