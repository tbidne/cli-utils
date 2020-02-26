{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Control.Concurrent.ParallelIO.Global
import           Control.Monad.Reader (runReaderT)
import qualified Data.Text as Txt
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (utctDay, getCurrentTime)
import           System.Environment (getArgs)

import Core
import Env
import GitUtils

currDay :: IO Day
currDay = fmap utctDay getCurrentTime

main :: IO ()
main = do
  args <- getArgs
  d <- currDay
  case mkEnv d args of
    Nothing  -> putStrLn "Usage: stack exec git-utils-exe <search str> <path> <limit>"
    Just env -> do
      runReaderT (runAppT runWithReader) env
      stopGlobalPool

mkEnv :: Day -> [String] -> Maybe Env
mkEnv d =
  \case
    [s]       -> Just $ Env (emptyTxt (Txt.pack s)) d 30 Nothing
    [s, p]    -> Just $ Env (emptyTxt (Txt.pack s)) d 30 $ emptyStr p
    [s, p, l] -> Just $ Env (emptyTxt (Txt.pack s)) d (read l :: Integer) $ emptyStr p
    _         -> Nothing
    where emptyTxt = guarded (/= Txt.empty)
          emptyStr = guarded (/= "")

guarded :: (a -> Bool) -> a -> Maybe a
guarded f x
  | f x   = Just x
  | otherwise = Nothing

-- stack test --profile