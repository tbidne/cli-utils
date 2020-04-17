{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module CLI.Parsing
  ( parseArgs,
  )
where

import CLI.Types.Env
import Common.Parsing.Core
import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import qualified Control.Exception as Ex
import qualified Data.Map.Strict as M
import qualified Data.Text as T

parseArgs :: [String] -> IO (Either ParseErr Env)
parseArgs args = do
  case parseAll allParsers args of
    ParseAnd (PFailure (Help _)) -> pure $ Left $ Help help
    ParseAnd (PFailure (Err arg)) ->
      pure $ Left $ Err $ "Could not parse `" <> arg <> "`. Try --help."
    ParseAnd (PSuccess acc) -> accToEnv acc

accToEnv :: Acc -> IO (Either ParseErr Env)
accToEnv Acc {legendPath, cmds} = do
  case legendPath of
    Nothing -> pure $ Right $ Env M.empty cmds
    Just path -> do
      res <- Ex.try (readFile path) :: IO (Either Ex.SomeException String)
      pure $ case res of
        Left err -> Left $ Err $ "Error reading file " <> show path <> ": " <> (show err)
        Right contents ->
          let eitherMap = linesToMap (T.lines (T.pack contents))
           in fmap (\mp -> Env mp cmds) eitherMap

linesToMap :: [T.Text] -> Either ParseErr (M.Map T.Text T.Text)
linesToMap = foldr f (Right M.empty)
  where
    f "" mp = mp
    f (T.stripPrefix "#" -> Just _) mp = mp
    f line mp = A.liftA2 insertPair (parseLine line) mp
    insertPair = \(key, cmd) mp -> (M.insert key cmd mp)

parseLine :: T.Text -> Either ParseErr (T.Text, T.Text)
parseLine l =
  case T.splitOn "=" l of
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
    parser s = Just $ T.pack s
    updater Acc {legendPath, cmds} c = Acc legendPath (c : cmds)

help :: String
help = error "TODO: CLI --help"
