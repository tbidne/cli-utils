{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.Arbitraries where

import Parsing.Internal
import Test.QuickCheck
import Types.Arbitraries ()

instance Arbitrary ArgHolder where
  arbitrary :: Gen ArgHolder
  arbitrary = do
    grp <- arbitrary
    p <- arbitrary
    lim <- arbitrary
    bt <- arbitrary
    return $ ArgHolder grp p lim bt

newtype ValidGrep = ValidGrep String deriving (Show)

instance Arbitrary ValidGrep where
  arbitrary = do
    (PrintableString s) <- arbitrary
    return $ ValidGrep $ "--grep=" <> s

newtype ValidPath = ValidPath String deriving (Show)

instance Arbitrary ValidPath where
  arbitrary = do
    (PrintableString p) <- arbitrary
    return $ ValidPath $ "--path=" <> p

newtype ValidLimit = ValidLimit String deriving (Show)

instance Arbitrary ValidLimit where
  arbitrary = do
    (NonNegative lim) <- arbitrary :: Gen (NonNegative Integer)
    return $ ValidLimit ("--limit=" <> show lim)

newtype ValidBranchType = ValidBranchType String deriving (Show)

newtype InValidLimit = InValidLimit String deriving (Show)

instance Arbitrary InValidLimit where
  arbitrary = do
    (Negative lim) <- arbitrary :: Gen (Negative Integer)
    return $ InValidLimit ("--limit=" <> show lim)

instance Arbitrary ValidBranchType where
  arbitrary = do
    bt <- elements ["all", "a", "r", "remote", "l", "local"]
    return $ ValidBranchType ("--branchType=" <> bt)

data ValidArgs
  = ValidArgs
      { validGrep :: String,
        validPath :: String,
        validLimit :: String,
        validBranchType :: String,
        order :: [String]
      }
  deriving (Show)

instance Arbitrary ValidArgs where
  arbitrary :: Gen ValidArgs
  arbitrary = do
    (ValidGrep g) <- arbitrary
    (ValidPath p) <- arbitrary
    (ValidLimit l) <- arbitrary
    (ValidBranchType b) <- arbitrary
    order' <- shuffle [g, p, l, b]
    return $ ValidArgs g p l b order'
