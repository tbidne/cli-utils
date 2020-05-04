{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Common.Utils
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports utility functions
module Common.Utils
  ( diffTime,
    divWithRem,
    eitherCompose,
    eitherComposeMay,
    eitherJoin,
    eitherToMaybe,
    formatSeconds,
    matchAndStrip,
    monoBimap,
    showToText,
    startsWith,
  )
where

import Common.RefinedUtils
import qualified Data.Bifunctor as BF
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

-- | Flipped version of 'startsWith'. Useful with @ViewPatterns@,
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
diffTime :: C.TimeSpec -> C.TimeSpec -> RNonNegative Int
diffTime t1 t2 =
  let diff = fromIntegral $ C.sec $ C.diffTimeSpec t1 t2
   in -- Safe because 'C.diffTimeSpec' guaranteed to be non-zero
      unsafeRef diff

-- | For \(n \ge 0, d > 0\), returns non-negative \((e, r)\) such that
--
-- \[
--    \begin{align}
--      de + r = n \\
--      r < n \\
--    \end{align}
-- \]
divWithRem :: Integral a => RNonNegative a -> RPositive a -> (a, a)
divWithRem n d = (n' `div` d', n' `rem` d')
  where
    n' = unrefine n
    d' = unrefine d

-- | For \(n \ge 0\) seconds, returns a 'T.Text' description of the minutes
-- and seconds.
formatSeconds :: RNonNegative Int -> T.Text
formatSeconds seconds =
  let d = $$(refineTH 60) :: RPositive Int
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

-- | Joins a composed 'Either' in the natural way.
eitherJoin :: Either a (Either b c) -> Either () c
eitherJoin (Right (Right x)) = Right x
eitherJoin _ = Left ()

-- | Natural transformation from @'Either' a@ to 'Maybe'.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- | Composes 'Either' functions.
eitherCompose :: (a -> Either b c) -> (c -> Either d e) -> a -> Either () e
eitherCompose f g x = eitherJoin (g <$> f x)

-- | Composition of 'eitherToMaybe' to 'eitherCompose'.
eitherComposeMay :: (a -> Either b c) -> (c -> Either d e) -> a -> Maybe e
eitherComposeMay f g = eitherToMaybe . eitherCompose f g

-- | Transforms a showable to 'T.Text'.
showToText :: Show a => a -> T.Text
showToText = T.pack . show
