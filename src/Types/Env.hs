module Types.Env
  ( Env(..)
  )
where

import           Data.Text                      ( Text )
import           Data.Time.Calendar             ( Day )
import           System.IO                      ( )

data Env = Env
  { grepStr :: Maybe Text
  , today :: Day
  , limit :: Integer
  , path :: Maybe FilePath
  }
