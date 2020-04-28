{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : CLI.Parsing.Internal
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Internal module for CLI parsing.
module CLI.Parsing.Internal
  ( Acc (..),
    mapStrToEnv,
    pureParseArgs,
  )
where

import CLI.Types.Env
import Common.Parsing.Core
import Common.Types.NonNegative
import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Read as R

-- | Parses arguments into @'ParseAnd' acc@.
pureParseArgs :: [String] -> ParseAnd Acc
pureParseArgs = parseAll allParsers

-- | Transforms a list of commands and 'String' into 'Right' 'Env'.
-- Each non-empty, non-comment (comments start with #) line in the
-- map string are expected to have the form @key=val@. If any parse
-- errors are encountered then 'Left' 'ParseErr' is returned.
mapStrToEnv :: [T.Text] -> Maybe (NonNegative Int) -> String -> Either ParseErr Env
mapStrToEnv cmds t contents =
  let eitherMap = linesToMap (T.lines (T.pack contents))
   in fmap (\mp -> Env mp t cmds) eitherMap

linesToMap :: [T.Text] -> Either ParseErr (M.Map T.Text T.Text)
linesToMap = foldr f (Right M.empty)
  where
    f "" mp = mp
    f (T.stripPrefix "#" -> Just _) mp = mp
    f line mp = A.liftA2 insertPair (parseLine line) mp
    insertPair (key, cmd) mp = M.insert key cmd mp

parseLine :: T.Text -> Either ParseErr (T.Text, T.Text)
parseLine l =
  case T.splitOn "=" l of
    ["", _] -> Left $ Err $ "Could not parse line `" <> T.unpack l <> "` from legend file"
    [_, ""] -> Left $ Err $ "Could not parse line `" <> T.unpack l <> "` from legend file"
    [key, cmd] -> Right (key, cmd)
    _ -> Left $ Err $ "Could not parse line `" <> T.unpack l <> "` from legend file"

-- | Monoid accumulator for CLI.
data Acc
  = Acc
      { -- | Path to the legend file.
        accLegend :: Maybe FilePath,
        -- | Maximum time to run commands
        accTimeout :: Maybe (NonNegative Int),
        -- | List of commands to run.
        accCommands :: [T.Text]
      }
  deriving (Show)

instance Semigroup Acc where
  (Acc l t c) <> (Acc l' t' c') = Acc (l <|> l') (t <|> t') (c <> c')

instance Monoid Acc where
  mempty = Acc Nothing Nothing []

allParsers :: [AnyParser Acc]
allParsers =
  [ pathParser,
    timeoutParser,
    cmdParser
  ]

pathParser :: AnyParser Acc
pathParser = AnyParser $ PrefixParser ("--legend=", parser, updater)
  where
    parser "" = Just Nothing
    parser s = Just $ Just s
    updater acc p = acc {accLegend = p}

timeoutParser :: AnyParser Acc
timeoutParser = AnyParser $ PrefixParser ("--timeout=", parser, updater)
  where
    parser "" = Just Nothing
    parser s =
      case R.readMaybe s >>= toNonNegative of
        Just t -> Just $ Just t
        Nothing -> Nothing
    updater acc t = acc {accTimeout = t}

cmdParser :: AnyParser Acc
cmdParser = AnyParser $ ExactParser (parser, updater)
  where
    parser = Just . T.pack
    updater acc@(Acc {accCommands}) c = acc {accCommands = (c : accCommands)}
