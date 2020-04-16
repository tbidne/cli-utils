{-# LANGUAGE GADTs #-}

-- |
-- Module      : Common.Parsing
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides functions for parsing `String` arguments.
module Common.Parsing
  ( AnyParser (..),
    Parser (..),
    ParseAnd (..),
    ParseErr (..),
    ParseStatus (..),
    parseAll,
  )
where

import Common.Utils

-- | Describes anything other than a successful parse.
data ParseErr
  = -- | Indicates that there was an error when trying to parse 'String'.
    Err String
  | -- | Indicates the argument "--help" was passed.
    Help String
  deriving (Show)

data ParseStatus acc
  = PFailure ParseErr
  | PSuccess acc
  deriving (Show)

newtype ParseOr acc = ParseOr (ParseStatus acc) deriving (Show)

instance Semigroup (ParseOr acc) where
  (ParseOr (PFailure _)) <> r = r
  l <> _ = l

newtype ParseAnd acc = ParseAnd (ParseStatus acc) deriving (Show)

instance Semigroup acc => Semigroup (ParseAnd acc) where
  (ParseAnd (PFailure l)) <> _ = ParseAnd $ PFailure l
  _ <> (ParseAnd (PFailure r)) = ParseAnd $ PFailure r
  (ParseAnd (PSuccess l)) <> (ParseAnd (PSuccess r)) = ParseAnd $ PSuccess $ l <> r

instance Monoid acc => Monoid (ParseAnd acc) where
  mempty = ParseAnd (PSuccess mempty)

parseOrToAnd :: ParseOr acc -> ParseAnd acc
parseOrToAnd (ParseOr x) = ParseAnd x

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

-- | GADT that hides the 'a' on 'Parser' 'a' 'acc'. This way we can accept
-- a heterogenous ['AnyParser' 'acc'] so we can try different parsers at once.
data AnyParser acc where
  AnyParser :: Parser a acc -> AnyParser acc

-- | Entrypoint for parsing arguments ['String'] into 'acc'. Returns
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
--       1, &p\mathrm{\,parsed\,}s\,\mathrm{successfully.} \\
--       0, &\mathrm{otherwise}
--     \end{cases}
--   \]
-- Then,
--   \[
--      \mathrm{parseAll}(P)(S) = \mathrm{Right\,acc} \iff \forall s \in S, \exists p \in P \mathrm{\;such\,that\;} p(s) = 1
--   \]
parseAll :: Monoid acc => [AnyParser acc] -> [String] -> ParseAnd acc
parseAll parsers = foldr f mempty
  where
    f arg acc = parseOrToAnd (tryParsers parsers arg) <> acc

tryParsers :: Monoid acc => [AnyParser acc] -> String -> ParseOr acc
tryParsers _ "--h" = ParseOr $ PFailure $ Help ""
tryParsers _ "--help" = ParseOr $ PFailure $ Help ""
tryParsers parsers arg = foldr f (ParseOr (PFailure (Err arg))) parsers
  where
    f (AnyParser p) attempt = tryParser p arg <> attempt

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
