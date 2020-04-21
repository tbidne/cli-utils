{-# LANGUAGE NamedFieldPuns #-}

module CLI.Parsing
  ( parseArgs,
  )
where

import CLI.Parsing.Internal
import CLI.Types.Env
import Common.Parsing.Core
import qualified Control.Exception as Ex
import qualified Data.Map.Strict as M

parseArgs :: [String] -> IO (Either ParseErr Env)
parseArgs args = do
  case pureParseArgs args of
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
        Right contents -> mapStrToEnv cmds contents

help :: String
help = error "TODO: CLI --help"