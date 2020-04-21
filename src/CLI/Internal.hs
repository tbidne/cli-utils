{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : CLI.Internal
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module CLI.Internal
  (translateCommands,
  )
where

import qualified Data.Map as Map
import qualified Data.Text as T

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