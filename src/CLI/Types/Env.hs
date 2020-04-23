module CLI.Types.Env
  (Env(..)
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Env = Env
  { legend :: M.Map T.Text T.Text,
    commands :: [T.Text]
  }
  deriving Show
