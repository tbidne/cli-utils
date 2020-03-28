{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Git.Stale.Parsing
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Handles parsing of String args into Env.
module Git.Stale.Parsing
  ( parseArgs,
  )
where

import Common.Parsing
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Git.Stale.Types.Env
import Git.Stale.Types.Nat
import qualified Text.Read as R

-- | Maps `Cal.Day` and parsed [`String`] args into `Right` `Env`, returning
-- any errors as `Left` `String`. All arguments are optional
-- (i.e. an empty list is valid), but if any are provided then they must
-- be valid or an error will be returned. Valid arguments are:
--
-- @
--   --grep=\<string\>
--       Used for filtering on branch names. Any `String` is fine, including
--       the empty string (i.e. --grep=). Defaults to the empty string.
--
--   --path=\<string>\
--       Path to the git directory. Any `String` is fine, including the empty
--       string (i.e. --path=). Defaults to /share.
--
--   --limit=\<days\>
--       Determines if a branch should be considered stale. Must be a
--       non-negative integer. Defaults to 30.
--
--   --branch-type=\<all|remote|local\>
--       Determines which branches we should search. Must be one of
--       [all, remote, local]. Defaults to remote.
--
--   --remote=\<string\>
--       Name of the remote, used for stripping out the the remote name for
--       display purposes. Any `String` is fine, including the empty string
--       (i.e. --remote=). Defaults to origin.
--
--   --master=\<string\>
--       Name of the branch to consider merges against. Any `String` is fine,
--       including the empty string (i.e. --master=). Defaults to origin/master.
-- @
parseArgs :: Cal.Day -> [String] -> Either String Env
parseArgs d args =
  case parseAll allParsers args (defaultEnv d) of
    Left Help -> Left help
    Left (Err arg) -> Left $ "Could not parse `" <> arg <> "`. Try --help."
    Right env -> Right env

defaultEnv :: Cal.Day -> Env
defaultEnv =
  Env
    Nothing
    Nothing
    (fromJust (mkNat 30))
    Remote
    "origin/"
    "origin/master"

allParsers :: [AnyParser Env]
allParsers =
  [ grepParser,
    pathParser,
    limitParser,
    branchTypeParser,
    branchAllFlagParser,
    branchRemoteFlagParser,
    branchLocalFlagParser,
    remoteNameParser,
    masterParser
  ]

grepParser :: AnyParser Env
grepParser = AnyParser $ PrefixParser ("--grep=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just $ T.pack s
    updater (Env _ p l b r m d) g' = Env g' p l b r m d

pathParser :: AnyParser Env
pathParser = AnyParser $ PrefixParser ("--path=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just s
    updater (Env g _ l b r m d) p' = Env g p' l b r m d

limitParser :: AnyParser Env
limitParser = AnyParser $ PrefixParser ("--limit=", parser, updater)
  where
    parser "" = Nothing
    parser s = R.readMaybe s >>= mkNat
    updater (Env g p _ b r m d) l' = Env g p l' b r m d

branchTypeParser :: AnyParser Env
branchTypeParser = AnyParser $ PrefixParser ("--branch-type=", parser, updater)
  where
    parser "all" = Just All
    parser "remote" = Just Remote
    parser "local" = Just Local
    parser _ = Nothing
    updater (Env g p l _ r m d) b' = Env g p l b' r m d

branchAllFlagParser :: AnyParser Env
branchAllFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-a" = Just All
    parser _ = Nothing
    updater (Env g p l _ r m d) b' = Env g p l b' r m d

branchRemoteFlagParser :: AnyParser Env
branchRemoteFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-r" = Just Remote
    parser _ = Nothing
    updater (Env g p l _ r m d) b' = Env g p l b' r m d

branchLocalFlagParser :: AnyParser Env
branchLocalFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-l" = Just Local
    parser _ = Nothing
    updater (Env g p l _ r m d) b' = Env g p l b' r m d

remoteNameParser :: AnyParser Env
remoteNameParser = AnyParser $ PrefixParser ("--remote=", parser, updater)
  where
    parser "" = Just ""
    parser s = Just $ T.pack (s <> "/")
    updater (Env g p l b _ m d) r' = Env g p l b r' m d

masterParser :: AnyParser Env
masterParser = AnyParser $ PrefixParser ("--master=", parser, updater)
  where
    parser "" = Just ""
    parser s = Just $ T.pack s
    updater (Env g p l b r _ d) m' = Env g p l b r m' d

help :: String
help =
  "\nUsage: git-utils find-stale [OPTIONS]\n\n"
    <> "Displays stale branches.\n\nOptions:\n"
    <> "  --grep=<string>\t\tFilters branch names based on <string>.\n\n"
    <> "  --path=<string>\t\tDirectory path, defaults to current directory.\n\n"
    <> "  --limit=<days>\t\tNon-negative integer s.t. branch is stale iff last_commit(branch) >= <days>\n\n"
    <> "  -a, --branch-type=all\t\tSearches all branches.\n\n"
    <> "  -r, --branch-type=remote\tSearches remote branches only. This is the default.\n\n"
    <> "  -l, --branch-type=local\tSearches local branches only.\n\n"
    <> "  --remote=<string>\t\tName of the remote, used for stripping when displaying. Defaults to origin.\n\n"
    <> "  --master=<string>\t\tName of the branch to consider merges against. Defaults to origin/master.\n\n"
