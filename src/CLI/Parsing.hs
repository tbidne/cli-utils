{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : CLI.Parsing
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Handles parsing of ['String'] args into 'Env'.
module CLI.Parsing
  ( parseArgs,
  )
where

import CLI.Parsing.Internal
import CLI.Types.Env
import Common.Parsing.Core
import qualified Control.Exception as Ex
import qualified Data.Map.Strict as M

-- | Maps parsed ['String'] args into 'IO' 'Right' 'Env', returning
-- any errors as `Left` `String`. All arguments are optional
-- (i.e. an empty list is valid), but if any are provided then they must
-- be valid or an error will be returned. Valid arguments are:
--
-- @
--   --legend=\<string>\
--       Path to the legend file.
--
--   \<string\>
--       Any other string is considered a command. If the command has
--       whitespace then it must be quoted or it will be considered
--       a separate command.
-- @
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
help =
  "\nUsage: cli-utils run-sh \"command 1\" \"command 2\" ... [OPTIONS]\n\n"
    <> "Runs shell commands concurrently. Stdout is swallowed, so there is currently\n"
    <> "no point to performing commands whose only effect is printing to stdout.\n\nOptions:\n"
    <> "  --legend=<string>\tPath to a legend file.\n"
    <> "\t\t\tLines are formatted <cmd_key>=<command value> (no angle brackets).\n"
    <> "\t\t\tEach line can be separated by as many new lines as desired, and comment lines start\n"
    <> "\t\t\twith a #. Command values themselves can include multiple commands delimited by\n"
    <> "\t\t\tcommas, and they may reference other commands. For instance, given a legend file:\n\n"
    <> "\t\t\t\tcmd1=echo \"command one\"\n\n"
    <> "\t\t\t\t# recursive references\n"
    <> "\t\t\t\tcmd2=cmd1\n"
    <> "\t\t\t\tcmd3=cmd2\n\n"
    <> "\t\t\t\tcmd4=command four\n\n"
    <> "\t\t\t\t# runs 3 and 4\n"
    <> "\t\t\t\tall=cmd3,cmd4,echo hi\n\n"
    <> "\t\t\tThen the command\n\n"
    <> "\t\t\t\tcli-utils run-sh --legend=path/to/legend all \"echo cat\"\n\n"
    <> "\t\t\twill run `echo \"command one\"`, `command four`, `echo hi` and `echo cat` concurrently."