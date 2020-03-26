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

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Git.Stale.Types.Env
import Git.Stale.Types.Nat
import qualified System.IO as IO
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
parseArgs d args = holderToEnv d holder
  where
    holder = foldr addArgToHolder (Right defaultHolder) args

data ArgHolder
  = ArgHolder
      { grepArg :: Maybe (Maybe T.Text),
        pathArg :: Maybe (Maybe IO.FilePath),
        limitArg :: Maybe Nat,
        branchTypeArg :: Maybe BranchType,
        remoteArg :: Maybe T.Text,
        master :: Maybe T.Text
      }
  deriving (Eq, Show)

holderToEnv :: Cal.Day -> Either String ArgHolder -> Either String Env
holderToEnv
  d
  ( Right
      (ArgHolder (Just g) (Just p) (Just l) (Just b) (Just r) (Just m))
    ) =
    Right $ Env g p l b r m d
holderToEnv _ (Right (ArgHolder Nothing _ _ _ _ _)) =
  Left "Bad format for [--grep=<string>]"
holderToEnv _ (Right (ArgHolder _ Nothing _ _ _ _)) =
  Left "Bad format for [--path=<path>]"
holderToEnv _ (Right (ArgHolder _ _ Nothing _ _ _)) =
  Left "Bad format for [--limit=<days>] where <days> is an integer"
holderToEnv _ (Right (ArgHolder _ _ _ Nothing _ _)) =
  Left "Bad format for [--branch-type=<all|remote|local>]"
holderToEnv _ (Right (ArgHolder _ _ _ _ Nothing _)) =
  Left "Bad format for [--remote=<string>]"
holderToEnv _ (Right (ArgHolder _ _ _ _ _ Nothing)) =
  Left "Bad format for [--master=<string>]"
holderToEnv _ (Left x) = Left x

updateGrep :: Maybe (Maybe T.Text) -> ArgHolder -> ArgHolder
updateGrep g' (ArgHolder _ p l b r m) = ArgHolder g' p l b r m

updatePath :: Maybe (Maybe IO.FilePath) -> ArgHolder -> ArgHolder
updatePath p' (ArgHolder g _ l b r m) = ArgHolder g p' l b r m

updateLimit :: Maybe Nat -> ArgHolder -> ArgHolder
updateLimit l' (ArgHolder g p _ b r m) = ArgHolder g p l' b r m

updateBranchType :: Maybe BranchType -> ArgHolder -> ArgHolder
updateBranchType b' (ArgHolder g p l _ r m) = ArgHolder g p l b' r m

updateRemote :: Maybe T.Text -> ArgHolder -> ArgHolder
updateRemote r' (ArgHolder g p l b _ m) = ArgHolder g p l b r' m

updateMaster :: Maybe T.Text -> ArgHolder -> ArgHolder
updateMaster m' (ArgHolder g p l b r _) = ArgHolder g p l b r m'

defaultHolder :: ArgHolder
defaultHolder =
  ArgHolder
    (Just Nothing)
    (Just (Just "./"))
    (mkNat 30)
    (Just Remote)
    (Just "origin/")
    (Just "origin/master")

addArgToHolder :: String -> Either String ArgHolder -> Either String ArgHolder
addArgToHolder (startsWith "--grep=" -> Just rest) h = fmap (updateGrep (parseGrep rest)) h
addArgToHolder (startsWith "--path=" -> Just rest) h = fmap (updatePath (parsePath rest)) h
addArgToHolder (startsWith "--limit=" -> Just rest) h = fmap (updateLimit (parseLimit rest)) h
addArgToHolder (startsWith "--branch-type=" -> Just rest) h = fmap (updateBranchType (parseBranchType rest)) h
addArgToHolder "-a" h = fmap (updateBranchType (Just All)) h
addArgToHolder "-r" h = fmap (updateBranchType (Just Remote)) h
addArgToHolder "-l" h = fmap (updateBranchType (Just Local)) h
addArgToHolder (startsWith "--remote=" -> Just rest) h = fmap (updateRemote (parseRemote rest)) h
addArgToHolder (startsWith "--master=" -> Just rest) h = fmap (updateMaster (parseMaster rest)) h
addArgToHolder "--help" _ = Left help
addArgToHolder s h = h >>= const (Left ("Unknown argument: `" <> s <> "`. Try --help "))

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith [] ys = Just ys
startsWith _ [] = Nothing
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = Nothing

parseGrep :: String -> Maybe (Maybe T.Text)
parseGrep "" = Just Nothing
parseGrep s = Just $ Just $ T.pack s

parsePath :: String -> Maybe (Maybe IO.FilePath)
parsePath "" = Just Nothing
parsePath s = Just $ Just s

parseLimit :: String -> Maybe Nat
parseLimit "" = Nothing
parseLimit s = R.readMaybe s >>= mkNat

parseBranchType :: String -> Maybe BranchType
parseBranchType "all" = Just All
parseBranchType "remote" = Just Remote
parseBranchType "local" = Just Local
parseBranchType _ = Nothing

parseRemote :: String -> Maybe T.Text
parseRemote "" = Just ""
parseRemote s = Just $ T.pack (s <> "/")

parseMaster :: String -> Maybe T.Text
parseMaster "" = Just ""
parseMaster s = Just $ T.pack s

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
