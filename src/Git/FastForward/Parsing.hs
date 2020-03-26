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
    holder = foldr addArgToHolder (Right defaultHolder) args

holderToEnv :: Either String ArgHolder -> Either String Env
holderToEnv (Right (ArgHolder (Just p) (Just mt))) =
  Right $ Env p mt
holderToEnv (Right (ArgHolder Nothing _)) =
  Left "Bad format for [--path=<string>]. See --help."
holderToEnv (Right (ArgHolder _ Nothing)) =
  Left "Bad format for [--merge-type=<upstream|master|<any>]. See --help."
holderToEnv (Left x) = Left x

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

addArgToHolder :: String -> Either String ArgHolder -> Either String ArgHolder
addArgToHolder (startsWith "--path=" -> Just rest) h = fmap (updatePath (parsePath rest)) h
addArgToHolder (startsWith "--merge-type=" -> Just rest) h = fmap (updateMergeType (parseMergeType rest)) h
addArgToHolder "-m" h = fmap (updateMergeType (Just Master)) h
addArgToHolder "-u" h = fmap (updateMergeType (Just Upstream)) h
addArgToHolder "--help" _ = Left help
addArgToHolder s h = h >>= const (Left ("Unknown argument: `" <> s <> "`. Try --help "))

parsePath :: String -> Maybe (Maybe FilePath)
parsePath "" = Just Nothing
parsePath s = Just $ Just s

parseMergeType :: String -> Maybe MergeType
parseMergeType "" = Nothing
parseMergeType "upstream" = Just Upstream
parseMergeType "master" = Just Master
parseMergeType o = Just $ Other $ Name $ T.pack o

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith [] ys = Just ys
startsWith _ [] = Nothing
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = Nothing

help :: String
help =
  "\nUsage: git-utils fastforward [OPTIONS]\n\n"
    <> "Fast-forwards all local branches with --ff-only.\n\nOptions:\n"
    <> "  --path=<string>\t\tDirectory path, defaults to current directory.\n\n"
    <> "  -u, --merge-type=upstream\tMerges each branches' upstream via @{u}. This is the default.\n\n"
    <> "  -m, --merge-type=master\tMerges origin/master into each branch.\n\n"
    <> "  --merge-type=<string>\t\tMerges branch given by <string> into each branch."
