module Common.Parsing.CoreSpec
  ( spec,
  )
where

import Common.Parsing.Core
import qualified Data.Monoid as Mon
import Test.Hspec
import qualified Text.Read as R

spec :: Spec
spec = do
  describe "Common.Parsing.CoreSpec" $ do
    it "Individual parsing success is lazy (succeed-fast)" $ do
      let (ParseAnd res) = parseAll [nameParser, undefined] ["--name=Maverick"]
      res `shouldBe` (PSuccess (Acc (Mon.Sum 0) "Maverick" False))
    it "Overall parsing failure is lazy (fail-fast)" $ do
      let (ParseAnd res) = parseAll allParsers ["bad-arg", undefined]
      res `shouldBe` (PFailure (Err "bad-arg"))
    it "Parses --help" $ do
      let (ParseAnd res) = parseAll allParsers ["--name=Maverick", "--help"]
      res `shouldBe` (PFailure (Help ""))
    it "Parses -h" $ do
      let (ParseAnd res) = parseAll allParsers ["--name=Maverick", "-h"]
      res `shouldBe` (PFailure (Help ""))
    it "Parses all args correctly" $ do
      let (ParseAnd res) = parseAll allParsers ["--name=Maverick", "--num=7", "-flag", "--num=3"]
      res `shouldBe` (PSuccess (Acc (Mon.Sum 10) "Maverick" True))
    it "Empty args returns identity" $ do
      let (ParseAnd res) = parseAll allParsers []
      res `shouldBe` (PSuccess mempty)

data Acc
  = Acc
      { num :: Mon.Sum Int,
        name :: String,
        flag :: Bool
      }
  deriving (Eq, Show)

instance Semigroup Acc where
  (Acc x n b) <> (Acc x' n' b') = Acc (x <> x') (n <> n') (b || b')

instance Monoid Acc where
  mempty = Acc mempty mempty False

allParsers :: [AnyParser Acc]
allParsers = [numParser, nameParser, flagParser]

numParser :: AnyParser Acc
numParser = AnyParser $ PrefixParser ("--num=", parser, updater)
  where
    parser = fmap Mon.Sum . R.readMaybe
    updater acc s = acc {num = s}

nameParser :: AnyParser Acc
nameParser = AnyParser $ PrefixParser ("--name=", parser, updater)
  where
    parser = Just
    updater acc n = acc {name = n}

flagParser :: AnyParser Acc
flagParser = AnyParser $ ExactParser (parser, updater)
  where
    parser "-flag" = Just True
    parser _ = Nothing
    updater acc b = acc {flag = b}
