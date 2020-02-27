{-# LANGUAGE OverloadedStrings #-}

module GitUtils.Internal
( parseLog
, parseAuthDateStr
, parseDay
, stale
, toInt
) where

import           Control.Monad
import qualified Data.Text as Txt
import           Data.Time.Calendar (Day, diffDays, fromGregorian)

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
parseDay (n, a, t) =
  case fmap toInt (Txt.splitOn "-" t) of
    [y, m, d] -> Right $ (n, a, fromGregorian (toInteger y) m d)
    _         -> Left $ ParseDate t

stale :: Integer -> Day -> NameAuthDay -> Bool
stale lim day (_, _, d) = diffDays day d > lim

toInt :: Txt.Text -> Int
toInt = read . Txt.unpack