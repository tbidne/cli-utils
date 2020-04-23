{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : CLI.Internal
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides a function that parses command keys into their values.
module CLI.Internal
  ( translateCommands,
  )
where

import qualified Data.Map as Map
import qualified Data.Text as T

-- | Returns a list of 'T.Text' commands, potentially transforming a
-- given string via the `Map.Map` legend.
--
-- For a string \(s = s_1,\ldots,s_n\), we split \(s\) by commas then recursively
-- search on each \(s_i\). We stop and return \(s_i\) when it does not exist
-- as a key in the map.
--
-- For example,
--
-- @
--   m = { "cmd1": "one", "cmd2": "two", "all": "cmd1,cmd2,other" }
--   translateCommands m ["all", "blah"] == ["one", "two", "other", "blah"]
-- @
translateCommands :: Map.Map T.Text T.Text -> [T.Text] -> [T.Text]
translateCommands mp = foldMap (lineToCommands mp)

lineToCommands :: Map.Map T.Text T.Text -> T.Text -> [T.Text]
lineToCommands mp line =
  case T.splitOn "," line of
    [l] ->
      case Map.lookup l mp of
        Just c -> lineToCommands mp c
        Nothing -> [l]
    xs -> xs >>= lineToCommands mp
