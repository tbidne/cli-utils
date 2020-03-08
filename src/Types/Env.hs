module Types.Env
  ( Env (..),
  )
where

import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import qualified System.IO as IO

data Env
  = Env
      { grepStr :: Maybe T.Text,
        today :: C.Day,
        limit :: Integer,
        path :: Maybe IO.FilePath
      }
