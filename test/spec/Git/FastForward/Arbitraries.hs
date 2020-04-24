module Git.FastForward.Arbitraries
  ( ValidPath (..),
    ValidMergeType (..),
    ValidArgs (..),
    InvalidArgs (..),
  )
where

import Test.QuickCheck

newtype ValidPath = ValidPath String deriving (Show)

instance Arbitrary ValidPath where
  arbitrary = do
    (PrintableString p) <- arbitrary
    pure $ ValidPath $ "--path=" <> p

newtype ValidMergeType = ValidMergeType String deriving (Show)

instance Arbitrary ValidMergeType where
  arbitrary = do
    (PrintableString p) <- arbitrary `suchThat` \(PrintableString s) -> s /= ""
    bt <-
      elements
        [ "--merge=upstream",
          "-u",
          "--merge=master",
          "-m",
          "--merge=" <> p
        ]
    pure $ ValidMergeType bt

data ValidArgs
  = ValidArgs
      { validPath :: String,
        validMergeType :: String,
        validDoFetch :: [String],
        order :: [String]
      }
  deriving (Show)

instance Arbitrary ValidArgs where
  arbitrary = do
    (ValidPath path) <- arbitrary
    (ValidMergeType mergeType) <- arbitrary
    -- gen ["--no-fetch"] or []
    fetchStr <- resize 1 $ listOf $ pure "--no-fetch"
    order' <- shuffle $ [path, mergeType] ++ fetchStr
    pure $ ValidArgs path mergeType fetchStr order'

newtype InvalidArgs = InvalidArgs [String] deriving (Show)

instance Arbitrary InvalidArgs where
  arbitrary = do
    (PrintableString s) <- arbitrary `suchThat` nonEmpty
    InvalidArgs <$> vectorOf 4 (return s)
    where
      nonEmpty = (/= "") . getPrintableString
