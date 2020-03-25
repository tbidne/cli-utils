{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Git.FastForward.Parsing
  ( parseArgs,
  )
where

import qualified Data.Text as T
import Git.FastForward.Types.Env
import Git.FastForward.Types.MergeType
import Git.Types.GitTypes

parseArgs :: [String] -> Either String Env
parseArgs args = holderToEnv holder
  where
    holder = foldr addArgToHolder (Just defaultHolder) args

holderToEnv :: Maybe ArgHolder -> Either String Env
holderToEnv (Just (ArgHolder (Just p) (Just mt))) =
  Right $ Env p mt
holderToEnv (Just (ArgHolder Nothing _)) =
  Left "Bad format for [--path=<string>]"
holderToEnv (Just (ArgHolder _ Nothing)) =
  Left "Bad format for [--merge-type=<u[pstream]|m[master]|<any>]"
holderToEnv Nothing =
  Left $
    "Bad argument. Valid args are "
      <> "[--path=<string>], "
      <> "[--merge-type=<u[pstream]|m[master]|<any>]"

data ArgHolder
  = ArgHolder
      { pathArg :: Maybe (Maybe FilePath),
        mergeTypeArg :: Maybe MergeType
      }
  deriving (Eq, Show)

updatePath :: Maybe (Maybe FilePath) -> ArgHolder -> ArgHolder
updatePath p' (ArgHolder _ mt) = ArgHolder p' mt

updateMergeType :: Maybe MergeType -> ArgHolder -> ArgHolder
updateMergeType mt' (ArgHolder p _) = ArgHolder p mt'

defaultHolder :: ArgHolder
defaultHolder =
  ArgHolder
    (Just (Just "./"))
    (Just Upstream)

addArgToHolder :: String -> Maybe ArgHolder -> Maybe ArgHolder
addArgToHolder (startsWith "--path=" -> Just rest) h = fmap (updatePath (parsePath rest)) h
addArgToHolder (startsWith "--merge-type=" -> Just rest) h = fmap (updateMergeType (parseMergeType rest)) h
addArgToHolder _ _ = Nothing

parsePath :: String -> Maybe (Maybe FilePath)
parsePath "" = Just Nothing
parsePath s = Just $ Just s

parseMergeType :: String -> Maybe MergeType
parseMergeType "" = Nothing
parseMergeType "upstream" = Just Upstream
parseMergeType "u" = Just Upstream
parseMergeType "master" = Just Master
parseMergeType "m" = Just Master
parseMergeType o = Just $ Other $ Name $ T.pack o

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith [] ys = Just ys
startsWith _ [] = Nothing
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = Nothing
