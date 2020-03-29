{-# LANGUAGE GADTs #-}

-- |
-- Module      : Common.Parsing
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides functions for parsing `String` arguments.
module Common.Parsing
  ( AnyParser (..),
    Parser (..),
    ParseErr (..),
    parseAll,
  )
where

import Common.Utils

-- | Describes anything other than a successful parse.
data ParseErr
  = -- | Indicates that there was an error when trying to parse 'String'.
    Err String
  | -- | Indicates the argument "--help" was passed.
    Help

data ParseAttempt acc
  = PFailure acc
  | PSuccess acc

instance Semigroup (ParseAttempt acc) where
  (PFailure _) <> (PSuccess y) = PSuccess y
  p <> _ = p

fromParseAttempt :: ParseAttempt a -> a
fromParseAttempt (PFailure x) = x
fromParseAttempt (PSuccess x) = x

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
parseAll :: [AnyParser acc] -> [String] -> acc -> Either ParseErr acc
parseAll parsers args acc = foldr f (Right acc) args
  where
    f _ (Left s) = Left s
    f arg (Right acc') = tryParsers parsers acc' arg

tryParsers :: [AnyParser acc] -> acc -> String -> Either ParseErr acc
tryParsers _ _ "--h" = Left Help
tryParsers _ _ "--help" = Left Help
tryParsers parsers acc arg =
  case foldr f (PFailure acc) parsers of
    PSuccess acc' -> Right acc'
    PFailure _ -> Left (Err arg)
  where
    f (AnyParser p) attempt = attempt <> tryParser p arg (fromParseAttempt attempt)

tryParser :: Parser a acc -> String -> acc -> ParseAttempt acc
tryParser (ExactParser (parseFn, updateFn)) arg acc =
  parseAndUpdate parseFn updateFn arg acc
tryParser (PrefixParser (prefix, parseFn, updateFn)) arg acc =
  case arg `startsWith` prefix of
    Just rest -> parseAndUpdate parseFn updateFn rest acc
    Nothing -> PFailure acc

parseAndUpdate ::
  (String -> Maybe a) ->
  (acc -> a -> acc) ->
  String ->
  acc ->
  ParseAttempt acc
parseAndUpdate parseFn updateFn arg acc =
  case parseFn arg of
    Just parsed -> PSuccess $ updateFn acc parsed
    Nothing -> PFailure acc
