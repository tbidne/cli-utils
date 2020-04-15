-- |
-- Module      : Common.Utils
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports utility functions
module Common.Utils
  ( diffTime,
    divWithRem,
    matchAndStrip,
    monoBimap,
    showToText,
    startsWith,
  )
where

import qualified Data.Bifunctor as BF
import qualified Data.Text as T
import qualified System.Clock as C

-- | Determines if the second parameter is a prefix of the first,
-- returns the rest if so. That is,
--
--   \[
--     \newcommand\doubleplus{+\kern-1.3ex+\kern0.8ex}
--     \mathrm{startsWith}(xs)(ys) = \begin{cases}
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

diffTime :: C.TimeSpec -> C.TimeSpec -> Int
diffTime t1 t2 = (toI t2) - (toI t1)
  where
    toI = fromIntegral . C.sec

divWithRem :: Integral a => a -> a -> (a, a)
divWithRem x d = (x `div` d, x `rem` d)

monoBimap :: BF.Bifunctor f => (a -> b) -> f a a -> f b b
monoBimap g = BF.bimap g g

showToText :: Show a => a -> T.Text
showToText = T.pack . show
