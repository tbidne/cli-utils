{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Common.Parsing.Core
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides functions for parsing `String` arguments.
module Common.Parsing.Core
  ( AnyParser (..),
    Parser (..),
    parseAll,
    module Common.Parsing.ParseAnd,
    module Common.Parsing.ParseOr,
  )
where

import Common.Parsing.ParseAnd
import Common.Parsing.ParseOr
import Common.Utils

-- | Wraps functions that:
--
--   1. Attempts to parse a String into an 'a'.
--   2. Updates 'acc' with 'a'.
--
--   That is,
--
-- @
--   parseFn :: 'String' -> 'Maybe' a
--   updateFn :: acc -> a -> acc
-- @
data Parser a acc
  = -- | Parses an exact argument e.g. "-flag".
    ExactParser (String -> Maybe a, acc -> a -> acc)
  | -- | Includes a prefix for parsing e.g. "--arg=val".
    PrefixParser (String, String -> Maybe a, acc -> a -> acc)

-- | Existentially quantifies the 'a' in 'Parser' 'a' 'acc'. This way we
-- can accept a heterogenous ['AnyParser' 'acc'] so we can try different
-- parsers at once.
data AnyParser acc = forall a. AnyParser (Parser a acc)

-- | Entrypoint for parsing arguments ['String'] into 'acc'. We impose a
-- monoid requirement on the accumulator to take advantage of laziness.
-- Returns
--
-- @
--    - 'Left' ('Err' 'String'): /some/ 'String' could not be parsed by /any/ parser.
--    - 'Left' 'Help': "--help" was found
--    - 'Right' 'acc': /every/ 'String' was parsed successfully by /some/ parser.
-- @
--
-- In symbols, let /P/ = ['AnyParser' 'acc'], /S/ = ['String'] and define
--   \[
--     p(s) = \begin{cases}
--       1, &p\text{ parsed } s \text{ successfully.} \\
--       0, &\mathrm{otherwise}
--     \end{cases}
--   \]
-- Then,
--   \[
--      \mathrm{parseAll}(P, S) = \text{Right acc} \iff \forall s \in S, \exists p \in P \text{ such that } p(s) = 1
--   \]
parseAll :: Monoid acc => [AnyParser acc] -> [String] -> ParseAnd acc
parseAll parsers = foldMap f
  where
    f arg = parseOrToAnd $ tryParsers parsers arg

tryParsers :: Monoid acc => [AnyParser acc] -> String -> ParseOr acc
tryParsers _ "-h" = ParseOr $ PFailure $ Help ""
tryParsers _ "--help" = ParseOr $ PFailure $ Help ""
tryParsers parsers arg = foldMap f parsers
  where
    f (AnyParser p) = tryParser p arg

tryParser :: Monoid acc => Parser a acc -> String -> ParseOr acc
tryParser (ExactParser (parseFn, updateFn)) arg =
  parseAndUpdate parseFn updateFn arg
tryParser (PrefixParser (prefix, parseFn, updateFn)) arg =
  case arg `startsWith` prefix of
    Just rest -> parseAndUpdate parseFn updateFn rest
    Nothing -> ParseOr $ PFailure $ Err arg

parseAndUpdate ::
  Monoid acc =>
  (String -> Maybe a) ->
  (acc -> a -> acc) ->
  String ->
  ParseOr acc
parseAndUpdate parseFn updateFn arg =
  case parseFn arg of
    Just parsed -> ParseOr $ PSuccess $ updateFn mempty parsed
    Nothing -> ParseOr $ PFailure $ Err arg

parseOrToAnd :: ParseOr acc -> ParseAnd acc
parseOrToAnd (ParseOr x) = ParseAnd x