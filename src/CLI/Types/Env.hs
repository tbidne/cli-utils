-- |
-- Module      : CLI.Types.Env
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides core 'Env' types used with CLI.
module CLI.Types.Env
  ( Env (..),
  )
where

import Common.Types.NonNegative
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Provides a list of commands and legend map.
data Env
  = Env
      { legend :: M.Map T.Text T.Text,
        timeout :: Maybe (NonNegative Int),
        commands :: [T.Text]
      }
  deriving (Show)
