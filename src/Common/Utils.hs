{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Common.Utils
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports utility functions
module Common.Utils
  ( diffTime,
    divWithRem,
    formatSeconds,
    matchAndStrip,
    monoBimap,
    showToText,
    startsWith,
  )
where

import Common.Types.NonNegative
import Common.Types.Positive
import qualified Data.Bifunctor as BF
import qualified Data.Maybe as May
import qualified Data.Text as T
import qualified System.Clock as C

-- | Determines if the second parameter is a prefix of the first,
-- returns the rest if so. That is,
--
--   \[
--     \newcommand\doubleplus{+\kern-1.3ex+\kern0.8ex}
--     \mathrm{startsWith}(xs, ys) = \begin{cases}
--       \mathrm{Just\,} zs, &xs = ys \doubleplus zs \\
--       \mathrm{Nothing}, &\mathrm{otherwise}
--     \end{cases}
--   \]
--
-- Can be called infix, e.g.
--
-- @
--   "hello world" `'startsWith'` "hello" --> 'Just' " world"
-- @
startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith xs [] = Just xs
startsWith [] _ = Nothing
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = Nothing

-- | Flipped version of 'startsWith'. Useful with 'ViewPatterns',
-- e.g.
--
-- @
--    :set -XViewPatterns
--
--    parseArg :: 'String' -> 'Either' 'String' SomeType
--    parseArg (matchAndStrip "--val=" -> 'Just' rest) = parseVal rest
--    parseArg (matchAndStrip "--other=" -> 'Just' rest) = parseOther rest
--    parseArg _ = 'Left' "did not match!"
-- @
matchAndStrip :: Eq a => [a] -> [a] -> Maybe [a]
matchAndStrip = flip startsWith

-- | For given \(x, y\), returns the absolute difference \(|x - y|\).
diffTime :: Integral a => C.TimeSpec -> C.TimeSpec -> NonNegative a
diffTime t1 t2 =
  let diff = fromIntegral $ C.sec $ C.diffTimeSpec t1 t2
   in -- Safe because 'C.diffTimeSpec' guaranteed to be non-zero
      May.fromJust $ toNonNegative diff

-- | For \(n \ge 0, d > 0\), returns non-negative \((e, r)\) such that
--
-- \[
--    \begin{align}
--      de + r = n \\
--      r < n \\
--    \end{align}
-- \]
divWithRem :: Integral a => NonNegative a -> Positive a -> (a, a)
divWithRem n d = (n' `div` d', n' `rem` d')
  where
    n' = getNonNegative n
    d' = getPositive d

-- | For \(n \ge 0\) seconds, returns a 'T.Text' description of the minutes
-- and seconds.
formatSeconds :: (Show a, Integral a) => NonNegative a -> T.Text
formatSeconds seconds =
  let d = May.fromJust $ toPositive 60
      (m, s) = divWithRem seconds d
      pluralize i t
        | i == 1 = t
        | otherwise = t <> "s"
   in T.concat
        [ showToText m,
          pluralize m " minute",
          " and ",
          showToText s,
          pluralize s " second",
          "  "
        ]

-- | Convenience function for mapping the same function over
-- a monomorphic bifunctor.
monoBimap :: BF.Bifunctor f => (a -> b) -> f a a -> f b b
monoBimap g = BF.bimap g g

showToText :: Show a => a -> T.Text
showToText = T.pack . show
