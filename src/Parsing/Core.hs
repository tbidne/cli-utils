module Parsing.Core
  ( parseArgs,
  )
where

import qualified Data.Time.Calendar as Cal
import Parsing.Internal
import Types.Env

parseArgs :: Cal.Day -> [String] -> Either String Env
parseArgs d args = holderToEnv d holder
  where
    holder = foldr addArgToHolder (Just defaultHolder) args
