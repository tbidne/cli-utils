{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Parsing.Core
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Handles parsing of String args into Env.
module Parsing.Core
  ( parseArgs,
  )
where

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import qualified System.IO as IO
import qualified Text.Read as R
import Types.Env
import Types.Nat

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
--   --branchType=\<a[ll]|r[emote]|l[ocal]\>
--       Determines which branches we should search. Must be one of
--       [a, all, r, remote, l, local]. Defaults to remote.
--
--   --remote=\<string>
--       Name of the remote, used for stripping out the the remote name for
--       display purposes. Any `String` is fine, including the empty string
--       (i.e. --remote=). Defaults to origin.
--
--   --master=\<string>
--       Name of the branch to consider merges against. Any `String` is fine,
--       including the empty string (i.e. --master=). Defaults to origin/master.
-- @
parseArgs :: Cal.Day -> [String] -> Either String Env
parseArgs d args = holderToEnv d holder
  where
    holder = foldr addArgToHolder (Just defaultHolder) args

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

holderToEnv :: Cal.Day -> Maybe ArgHolder -> Either String Env
holderToEnv d (Just (ArgHolder (Just g) (Just p) (Just l) (Just b) (Just r) (Just m))) =
  Right $ Env g p l b r m d
holderToEnv _ (Just (ArgHolder Nothing _ _ _ _ _)) =
  Left "Bad format for [--grep=<string>]"
holderToEnv _ (Just (ArgHolder _ Nothing _ _ _ _)) =
  Left "Bad format for [--path=<path>]"
holderToEnv _ (Just (ArgHolder _ _ Nothing _ _ _)) =
  Left "Bad format for [--limit=<days>] where <days> is an integer"
holderToEnv _ (Just (ArgHolder _ _ _ Nothing _ _)) =
  Left "Bad format for [--branches=<a[ll]|r[emote]|l[ocal]>]"
holderToEnv _ (Just (ArgHolder _ _ _ _ Nothing _)) =
  Left "Bad format for [--remote=<string>]"
holderToEnv _ (Just (ArgHolder _ _ _ _ _ Nothing)) =
  Left "Bad format for [--master=<string>]"
holderToEnv _ Nothing =
  Left $
    "Bad argument. Valid args are "
      <> "[--grep=<string>], "
      <> "[--path=<path>], "
      <> "[--limit=<days>], "
      <> "[--branchType=<a[ll]|r[emote]|l[ocal]>], "
      <> "[--remote=<string>], "
      <> "[--master=<string>]"

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
    (Just (Just "/share"))
    (mkNat 30)
    (Just Remote)
    (Just "origin/")
    (Just "origin/master")

addArgToHolder :: String -> Maybe ArgHolder -> Maybe ArgHolder
addArgToHolder (startsWith "--grep=" -> Just rest) h = fmap (updateGrep (parseGrep rest)) h
addArgToHolder (startsWith "--path=" -> Just rest) h = fmap (updatePath (parsePath rest)) h
addArgToHolder (startsWith "--limit=" -> Just rest) h = fmap (updateLimit (parseLimit rest)) h
addArgToHolder (startsWith "--branchType=" -> Just rest) h = fmap (updateBranchType (parseBranchType rest)) h
addArgToHolder (startsWith "--remote=" -> Just rest) h = fmap (updateRemote (parseRemote rest)) h
addArgToHolder (startsWith "--master=" -> Just rest) h = fmap (updateMaster (parseMaster rest)) h
addArgToHolder _ _ = Nothing

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
parseBranchType "a" = Just All
parseBranchType "all" = Just All
parseBranchType "r" = Just Remote
parseBranchType "remote" = Just Remote
parseBranchType "l" = Just Local
parseBranchType "local" = Just Local
parseBranchType _ = Nothing

parseRemote :: String -> Maybe T.Text
parseRemote "" = Just ""
parseRemote s = Just $ T.pack (s <> "/")

parseMaster :: String -> Maybe T.Text
parseMaster "" = Just ""
parseMaster s = Just $ T.pack s
