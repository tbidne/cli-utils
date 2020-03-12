{-# LANGUAGE OverloadedStrings #-}

module Parsing.Arbitraries where

import Test.QuickCheck
import Types.Arbitraries ()

newtype ValidGrep = ValidGrep String deriving (Show)

instance Arbitrary ValidGrep where
  arbitrary = do
    (PrintableString s) <- arbitrary
    pure $ ValidGrep $ "--grep=" <> s

newtype ValidPath = ValidPath String deriving (Show)

instance Arbitrary ValidPath where
  arbitrary = do
    (PrintableString p) <- arbitrary
    pure $ ValidPath $ "--path=" <> p

newtype ValidLimit = ValidLimit String deriving (Show)

instance Arbitrary ValidLimit where
  arbitrary = do
    (NonNegative lim) <- arbitrary :: Gen (NonNegative Integer)
    pure $ ValidLimit ("--limit=" <> show lim)

newtype InvalidLimit = InvalidLimit String deriving (Show)

instance Arbitrary InvalidLimit where
  arbitrary = do
    (Negative lim) <- arbitrary :: Gen (Negative Integer)
    pure $ InvalidLimit ("--limit=" <> show lim)

newtype ValidBranchType = ValidBranchType String deriving (Show)

instance Arbitrary ValidBranchType where
  arbitrary = do
    bt <- elements ["all", "a", "r", "remote", "l", "local"]
    pure $ ValidBranchType ("--branchType=" <> bt)

newtype InvalidBranchType = InvalidBranchType String deriving (Show)

instance Arbitrary InvalidBranchType where
  arbitrary = do
    (PrintableString s) <- arbitrary `suchThat` nonEmpty
    pure $ InvalidBranchType ("--branchType=" <> s)
    where
      nonEmpty (PrintableString t) = not $ t `elem` ["a", "r", "l"]

newtype ValidRemoteName = ValidRemoteName String deriving (Show)

instance Arbitrary ValidRemoteName where
  arbitrary = do
    (PrintableString s) <- arbitrary
    pure $ ValidRemoteName $ "--remote=" <> s

newtype ValidMaster = ValidMaster String deriving (Show)

instance Arbitrary ValidMaster where
  arbitrary = do
    (PrintableString s) <- arbitrary
    pure $ ValidMaster $ "--master=" <> s

data ValidArgs
  = ValidArgs
      { validGrep :: String,
        validPath :: String,
        validLimit :: String,
        validBranchType :: String,
        validRemoteName :: String,
        validMaster :: String,
        order :: [String]
      }
  deriving (Show)

instance Arbitrary ValidArgs where
  arbitrary = do
    (ValidGrep g) <- arbitrary
    (ValidPath p) <- arbitrary
    (ValidLimit l) <- arbitrary
    (ValidBranchType b) <- arbitrary
    (ValidRemoteName r) <- arbitrary
    (ValidMaster m) <- arbitrary
    order' <- shuffle [g, p, l, b, r, m]
    pure $ ValidArgs g p l b r m order'

newtype InvalidArgs = InvalidArgs [String] deriving (Show)

instance Arbitrary InvalidArgs where
  arbitrary = do
    (PrintableString s) <- (arbitrary) `suchThat` nonEmpty
    InvalidArgs <$> vectorOf 4 (return s)
    where
      nonEmpty = not . (/= "") . getPrintableString
