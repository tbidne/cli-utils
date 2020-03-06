{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Core.Internal
  ( badBranch
  , exceptToErr
  , parseLog
  , parseAuthDateStr
  , parseDay
  , stale
  , staleNonErr
  , safeRead
  , toInt
  )
where

import           Control.Monad ((>=>))
import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import qualified Text.Read as R

import           Types.Error
import           Types.GitTypes

parseLog :: NameLog -> ErrOr NameAuthDay
parseLog = parseAuthDateStr >=> parseDay

parseAuthDateStr :: NameLog -> ErrOr NameAuthDateStr
parseAuthDateStr (n, l) = case T.splitOn "|" l of
  [a, t] -> Right (n, Author a, t)
  _      -> Left $ ParseLog l

parseDay :: NameAuthDateStr -> ErrOr NameAuthDay
parseDay (n, a, t) = fmap (n, a, ) eitherDay
 where
  eitherDay = case traverse safeRead (T.splitOn "-" t) of
    Right [y, m, d] -> Right $ C.fromGregorian (toInteger y) m d
    Right xs        -> Left $ ParseDate $ T.pack $ show xs
    Left  x         -> Left x

stale :: Integer -> C.Day -> NameAuthDay -> Bool
stale lim day (_, _, d) = C.diffDays day d > lim

staleNonErr :: Integer -> C.Day -> ErrOr NameAuthDay -> Bool
staleNonErr _ _ (Left  _  ) = True
staleNonErr i d (Right nad) = stale i d nad

toInt :: T.Text -> Int
toInt = read . T.unpack

safeRead :: T.Text -> ErrOr Int
safeRead t = case R.readMaybe (T.unpack t) of
  Nothing -> Left $ ReadInt t
  Just i  -> Right i

exceptToErr :: Show a => Either a (Either Err b) -> Either Err b
exceptToErr (Left  x) = Left $ GitLog $ T.pack (show x)
exceptToErr (Right r) = r

badBranch :: T.Text -> Bool
badBranch s = foldr f False ['*', '>']
  where f badChar b = b || T.any (== badChar) s