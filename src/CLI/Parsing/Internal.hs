{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module CLI.Parsing.Internal
  ( Acc (..),
    mapStrToEnv,
    pureParseArgs,
  )
where

import CLI.Types.Env
import Common.Parsing.Core
import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import qualified Data.Map as M
import qualified Data.Text as T

pureParseArgs :: [String] -> ParseAnd Acc
pureParseArgs = parseAll allParsers

mapStrToEnv :: [T.Text] -> String -> Either ParseErr Env
mapStrToEnv commands contents =
  let eitherMap = linesToMap (T.lines (T.pack contents))
   in fmap (`Env` commands) eitherMap

linesToMap :: [T.Text] -> Either ParseErr (M.Map T.Text T.Text)
linesToMap = foldr f (Right M.empty)
  where
    f "" mp = mp
    f (T.stripPrefix "#" -> Just _) mp = mp
    f line mp = A.liftA2 insertPair (parseLine line) mp
    insertPair (key, cmd) mp = (M.insert key cmd mp)

parseLine :: T.Text -> Either ParseErr (T.Text, T.Text)
parseLine l =
  case T.splitOn "=" l of
    ["", _] -> Left $ Err $ "Could not parse line `" <> T.unpack l <> "` from legend file"
    [_, ""] -> Left $ Err $ "Could not parse line `" <> T.unpack l <> "` from legend file"
    [key, cmd] -> Right (key, cmd)
    _ -> Left $ Err $ "Could not parse line `" <> T.unpack l <> "` from legend file"

data Acc
  = Acc
      { legendPath :: Maybe FilePath,
        cmds :: [T.Text]
      }
  deriving (Show)

instance Semigroup Acc where
  (Acc l c) <> (Acc l' c') = Acc (l <|> l') (c <> c')

instance Monoid Acc where
  mempty = Acc Nothing []

allParsers :: [AnyParser Acc]
allParsers =
  [ pathParser,
    cmdParser
  ]

pathParser :: AnyParser Acc
pathParser = AnyParser $ PrefixParser ("--legend=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just s
    updater acc p = acc {legendPath = p}

cmdParser :: AnyParser Acc
cmdParser = AnyParser $ ExactParser (parser, updater)
  where
    parser = Just . T.pack
    updater Acc {legendPath, cmds} c = Acc legendPath (c : cmds)