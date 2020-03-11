{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Parsing.Internal
  ( ArgHolder (..),
    addArgToHolder,
    defaultHolder,
    holderToEnv,
  )
where

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import qualified System.IO as IO
import qualified Text.Read as R
import Types.Env

data ArgHolder
  = ArgHolder
      { grepArg :: Maybe (Maybe T.Text),
        pathArg :: Maybe (Maybe IO.FilePath),
        limitArg :: Maybe Nat,
        branchTypeArg :: Maybe BranchType
      }
  deriving (Eq, Show)

holderToEnv :: Cal.Day -> Maybe ArgHolder -> Either String Env
holderToEnv d (Just (ArgHolder (Just g) (Just p) (Just l) (Just b))) =
  Right $ Env g p l b d
holderToEnv _ (Just (ArgHolder Nothing _ _ _)) =
  Left "Bad format for [--grep=<string>]"
holderToEnv _ (Just (ArgHolder _ Nothing _ _)) =
  Left "Bad format for [--path=<path>]"
holderToEnv _ (Just (ArgHolder _ _ Nothing _)) =
  Left "Bad format for [--limit=<days>] where <days> is an integer."
holderToEnv _ (Just (ArgHolder _ _ _ Nothing)) =
  Left "Bad format for [--branches=<a[ll]|r[emote]|l[ocal]>]."
holderToEnv _ Nothing =
  Left $
    "Bad argument. Valid args are"
      <> "[--grep=<string>], "
      <> "[--path=<path>], "
      <> "[--limit=<days>], "
      <> "and [--branchType=<a[ll]|r[emote]|l[ocal]>]"

updateGrep :: Maybe (Maybe T.Text) -> ArgHolder -> ArgHolder
updateGrep g' (ArgHolder _ p l b) = ArgHolder g' p l b

updatePath :: Maybe (Maybe IO.FilePath) -> ArgHolder -> ArgHolder
updatePath p' (ArgHolder g _ l b) = ArgHolder g p' l b

updateLimit :: Maybe Nat -> ArgHolder -> ArgHolder
updateLimit l' (ArgHolder g p _ b) = ArgHolder g p l' b

updateBranchType :: Maybe BranchType -> ArgHolder -> ArgHolder
updateBranchType b' (ArgHolder g p l _) = ArgHolder g p l b'

defaultHolder :: ArgHolder
defaultHolder = ArgHolder (Just Nothing) (Just (Just "/share")) (mkNat 30) (Just Remote)

addArgToHolder :: String -> Maybe ArgHolder -> Maybe ArgHolder
addArgToHolder (startsWith "--grep=" -> Just rest) h = fmap (updateGrep (parseGrep rest)) h
addArgToHolder (startsWith "--path=" -> Just rest) h = fmap (updatePath (parsePath rest)) h
addArgToHolder (startsWith "--limit=" -> Just rest) h = fmap (updateLimit (parseLimit rest)) h
addArgToHolder (startsWith "--branchType=" -> Just rest) h = fmap (updateBranchType (parseBranchType rest)) h
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
