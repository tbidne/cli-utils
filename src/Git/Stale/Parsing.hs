{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.Stale.Parsing
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Handles parsing of String args into 'Env'.
module Git.Stale.Parsing
  ( parseArgs,
  )
where

import Common.Parsing.Core
import Control.Applicative ((<|>))
import qualified Data.Maybe as May
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Git.Stale.Types.Env
import Common.Types.NonNegative
import qualified System.IO as IO
import qualified Text.Read as R

-- | Maps `Cal.Day` and parsed [`String`] args into `Right` `Env`, returning
-- any errors as `Left` `String`. All arguments are optional
-- (i.e. an empty list is valid), but if any are provided then they must
-- be valid or an error will be returned. Valid arguments are:
--
-- @
--   --grep=\<string\>
--       Used for filtering on branch names. Any `String` is fine,
--       defaults to the empty string.
--
--   --path=\<string>\
--       Path to the git directory. Any `String` is fine, defaults
--       to the empty string (current directory).
--
--   --limit=\<days\>
--       Determines if a branch should be considered stale. Must be a
--       non-negative integer. Defaults to 30.
--
--   -a, --branch-type=all
--       Searches local and remote branches.
--
--   -r, --branch-type=remote
--       Searches remote branches only. This is the default.
--
--   -l, --branch-type=local
--       Searches local branches only.
--
--   --remote=\<string\>
--       Name of the remote, used for stripping out the the remote name for
--       display purposes. Any `String` is fine, including the empty string.
--       Defaults to origin/.
--
--   --master=\<string\>
--       Name of the branch to consider merges against. Any `String` is fine,
--       including the empty string. Defaults to origin/master.
--
--  -h, --help
--       Returns instructions as `Left` `String`.
-- @
parseArgs :: Cal.Day -> [String] -> Either ParseErr Env
parseArgs d args =
  case parseAll allParsers args of
    ParseAnd (PFailure (Help _)) -> Left $ Help help
    ParseAnd (PFailure (Err arg)) ->
      Left $ Err $ "Could not parse `" <> arg <> "`. Try --help."
    ParseAnd (PSuccess acc) -> Right $ accToEnv d acc

newtype AccLimit = AccLimit (NonNegative Int) deriving (Show)

instance Semigroup AccLimit where
  (AccLimit l) <> r
    | (getNonNegative l == 30) = r
    | otherwise = (AccLimit l)

instance Monoid AccLimit where
  mempty = AccLimit $ May.fromJust $ toNonNegative 30

newtype AccBranchType = AccBranchType BranchType deriving (Show)

instance Semigroup AccBranchType where
  (AccBranchType Remote) <> r = r
  l <> _ = l

instance Monoid AccBranchType where
  mempty = AccBranchType Remote

newtype AccRemoteName = AccRemoteName T.Text deriving (Show)

instance Semigroup AccRemoteName where
  (AccRemoteName "origin/") <> r = r
  l <> _ = l

instance Monoid AccRemoteName where
  mempty = AccRemoteName "origin/"

newtype AccMaster = AccMaster T.Text deriving (Show)

instance Semigroup AccMaster where
  (AccMaster "origin/master") <> r = r
  l <> _ = l

instance Monoid AccMaster where
  mempty = AccMaster "origin/master"

data Acc
  = Acc
      { accGrep :: Maybe T.Text,
        accPath :: Maybe IO.FilePath,
        accLimit :: AccLimit,
        accBranchType :: AccBranchType,
        accRemoteName :: AccRemoteName,
        accMaster :: AccMaster
      }

instance Semigroup Acc where
  Acc
    { accGrep = g,
      accPath = p,
      accLimit = l,
      accBranchType = b,
      accRemoteName = r,
      accMaster = m
    }
    <> Acc
      { accGrep = g',
        accPath = p',
        accLimit = l',
        accBranchType = b',
        accRemoteName = r',
        accMaster = m'
      } =
      Acc (g <|> g') (p <|> p') (l <> l') (b <> b') (r <> r') (m <> m')

instance Monoid Acc where
  mempty = Acc mempty mempty mempty mempty mempty mempty

accToEnv :: Cal.Day -> Acc -> Env
accToEnv
  d
  ( Acc
      { accGrep,
        accPath,
        accLimit = (AccLimit l),
        accBranchType = (AccBranchType b),
        accRemoteName = (AccRemoteName r),
        accMaster = (AccMaster m)
      }
    ) =
    Env
      { grepStr = accGrep,
        path = accPath,
        limit = l,
        branchType = b,
        remoteName = r,
        master = m,
        today = d
      }

allParsers :: [AnyParser Acc]
allParsers =
  [ grepParser,
    pathParser,
    limitParser,
    branchTypeParser,
    branchFlagParser,
    remoteNameParser,
    masterParser
  ]

grepParser :: AnyParser Acc
grepParser = AnyParser $ PrefixParser ("--grep=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just $ T.pack s
    updater acc g = acc {accGrep = g}

pathParser :: AnyParser Acc
pathParser = AnyParser $ PrefixParser ("--path=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just s
    updater acc p = acc {accPath = p}

limitParser :: AnyParser Acc
limitParser = AnyParser $ PrefixParser ("--limit=", parser, updater)
  where
    parser "" = Nothing
    parser s = fmap AccLimit (R.readMaybe s >>= toNonNegative)
    updater acc l = acc {accLimit = l}

branchTypeParser :: AnyParser Acc
branchTypeParser = AnyParser $ PrefixParser ("--branch-type=", parser, updater)
  where
    parser "all" = Just $ AccBranchType All
    parser "remote" = Just $ AccBranchType Remote
    parser "local" = Just $ AccBranchType Local
    parser _ = Nothing
    updater acc b = acc {accBranchType = b}

branchFlagParser :: AnyParser Acc
branchFlagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-a" = Just $ AccBranchType All
    parser "-r" = Just $ AccBranchType Remote
    parser "-l" = Just $ AccBranchType Local
    parser _ = Nothing
    updater acc b = acc {accBranchType = b}

remoteNameParser :: AnyParser Acc
remoteNameParser = AnyParser $ PrefixParser ("--remote=", parser, updater)
  where
    parser "" = Just $ AccRemoteName ""
    parser s = Just $ AccRemoteName $ T.pack (s <> "/")
    updater acc r = acc {accRemoteName = r}

masterParser :: AnyParser Acc
masterParser = AnyParser $ PrefixParser ("--master=", parser, updater)
  where
    parser "" = Just $ AccMaster ""
    parser s = Just $ AccMaster $ T.pack s
    updater acc m = acc {accMaster = m}

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
