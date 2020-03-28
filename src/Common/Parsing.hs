{-# LANGUAGE GADTs #-}

module Common.Parsing
  ( AnyParser (..),
    Parser (..),
    ParseErr (..),
    parseAll,
  )
where

import Common.Utils

data ParseErr
  = Err String
  | Help

data ParseAttempt acc
  = PFailure acc
  | PSuccess acc

instance Semigroup (ParseAttempt acc) where
  (PFailure _) <> (PSuccess y) = PSuccess y
  p <> _ = p

fromParseAttempt :: ParseAttempt a -> a
fromParseAttempt (PFailure x) = x
fromParseAttempt (PSuccess x) = x

data Parser a acc
  = PrefixParser (String, String -> Maybe a, acc -> a -> acc)
  | ExactParser (String -> Maybe a, acc -> a -> acc)

data AnyParser acc where
  AnyParser :: Parser a acc -> AnyParser acc

parseAll :: [AnyParser acc] -> [String] -> acc -> Either ParseErr acc
parseAll parsers args acc = foldr f (Right acc) args
  where
    f _ (Left s) = Left s
    f arg (Right acc') = tryParsers parsers acc' arg

tryParsers :: [AnyParser acc] -> acc -> String -> Either ParseErr acc
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
