module Types.Env
  ( BranchType (..),
    Env (..),
    Nat,
    branchTypeToArg,
    mkNat,
    unNat,
  )
where

import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import qualified System.IO as IO
import Types.GitTypes

data BranchType
  = All
  | Remote
  | Local
  deriving (Eq, Show)

branchTypeToArg :: BranchType -> String
branchTypeToArg All = "-a"
branchTypeToArg Remote = "-r"
branchTypeToArg Local = ""

data Env
  = Env
      { grepStr :: Maybe T.Text,
        path :: Maybe IO.FilePath,
        limit :: Nat,
        branchType :: BranchType,
        today :: C.Day
      }
  deriving (Show)
