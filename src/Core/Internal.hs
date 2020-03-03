{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Core.Internal
( parseLog
, parseAuthDateStr
, parseDay
, stale
, toInt
, safeRead
) where

import           Control.Monad
import qualified Data.Text as Txt
import           Data.Time.Calendar (Day, diffDays, fromGregorian)
import           Text.Read (readMaybe)

import Types.Error
import Types.GitTypes

parseLog :: NameLog -> Either Err NameAuthDay
parseLog = parseAuthDateStr >=> parseDay

parseAuthDateStr :: NameLog -> Either Err NameAuthDateStr
parseAuthDateStr (n, l) =
  case Txt.splitOn "|" l of
    [a, t] -> Right (n, Author a, t)
    _      -> Left $ ParseLog l

parseDay :: NameAuthDateStr -> Either Err NameAuthDay
parseDay (n, a, t) = fmap (n,a,) eitherDay
  where eitherDay =
          case traverse safeRead (Txt.splitOn "-" t) of
            Right [y, m, d] -> Right $ fromGregorian (toInteger y) m d
            Right xs        -> Left $ ParseDate $ Txt.pack $ show xs
            Left x          -> Left x

stale :: Integer -> Day -> NameAuthDay -> Bool
stale lim day (_, _, d) = diffDays day d > lim

toInt :: Txt.Text -> Int
toInt = read . Txt.unpack

safeRead :: Txt.Text -> Either Err Int
safeRead t =
  case readMaybe (Txt.unpack t) of
    Nothing -> Left $ ReadInt t
    Just i  -> Right i