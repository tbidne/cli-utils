{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Core.Internal
  ( exceptToErr
  , parseLog
  , parseAuthDateStr
  , parseDay
  , stale
  , staleNonErr
  , safeRead
  , toInt
  )
where

import           Control.Monad
import qualified Data.Text                     as Txt
import           Data.Time.Calendar             ( Day
                                                , diffDays
                                                , fromGregorian
                                                )
import           Text.Read                      ( readMaybe )

import           Types.Error
import           Types.GitTypes

parseLog :: NameLog -> ErrOr NameAuthDay
parseLog = parseAuthDateStr >=> parseDay

parseAuthDateStr :: NameLog -> ErrOr NameAuthDateStr
parseAuthDateStr (n, l) = case Txt.splitOn "|" l of
  [a, t] -> Right (n, Author a, t)
  _      -> Left $ ParseLog l

parseDay :: NameAuthDateStr -> ErrOr NameAuthDay
parseDay (n, a, t) = fmap (n, a, ) eitherDay
 where
  eitherDay = case traverse safeRead (Txt.splitOn "-" t) of
    Right [y, m, d] -> Right $ fromGregorian (toInteger y) m d
    Right xs        -> Left $ ParseDate $ Txt.pack $ show xs
    Left  x         -> Left x

stale :: Integer -> Day -> NameAuthDay -> Bool
stale lim day (_, _, d) = diffDays day d > lim

staleNonErr :: Integer -> Day -> ErrOr NameAuthDay -> Bool
staleNonErr _ _ (Left  _  ) = True
staleNonErr i d (Right nad) = stale i d nad

toInt :: Txt.Text -> Int
toInt = read . Txt.unpack

safeRead :: Txt.Text -> ErrOr Int
safeRead t = case readMaybe (Txt.unpack t) of
  Nothing -> Left $ ReadInt t
  Just i  -> Right i

exceptToErr :: Show a => Either a (Either Err b) -> Either Err b
exceptToErr (Left  x) = Left $ GitLog $ Txt.pack (show x)
exceptToErr (Right r) = r
