{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests (
  tests
, testId
) where

import Control.Applicative
import Data.Increments
import Data.IntMap    (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet    (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

-- test that creating then applying a diff is equivalent to the identity
-- function
testId :: (Incremental a, Eq a) => a -> a -> Bool
testId prev this =
  let diff = changes prev this
      new  = applyChanges prev diff
  in this == new

tests :: [Test]
tests =
  [ testGroup "primitive types"
      [ testProperty "Int"     $ idForType pInt
      , testProperty "Char"    $ idForType pChar
      , testProperty "Integer" $ idForType pInteger
      ]
  , testGroup "Tuples"
      [ testProperty "(Int,Int)" $ idForType $ pTup pInt pInt
      , testProperty "(Int,Char)" $ idForType $ pTup pInt pInt
      ]
  , testProperty "Maybe Int" $ idForType $ pMaybe pInt
  , testGroup "Either"
      [ testProperty "Either Int Char" $ idForType $ pEither pInt pChar
      , testProperty "Either Integer Integer" $ idForType $ pEither pInteger pInteger
      ]
  , testGroup "Containers"
      [ testProperty "String" $ idForType $ pList pChar
      , testProperty "[Integer]" $ idForType $ pList pInteger
      , testProperty "Map String Integer" $ idForType $ pMap (pList pChar) pInteger
      , testProperty "IntMap String" $ idForType $ pIntMap (pList pChar)
      , testProperty "Set [Int]" $ idForType $ pSet (pList pInt)
      , testProperty "IntSet" $ idForType pIntSet
      ]
  ]
  
data Proxy p = Proxy deriving (Functor)

idForType :: (Eq p, Incremental p) => Proxy p -> p -> p -> Bool
idForType _ = testId

pInt :: Proxy Int
pInt = Proxy

pChar :: Proxy Char
pChar = Proxy

pInteger :: Proxy Integer
pInteger = Proxy

pTup :: Proxy a -> Proxy b -> Proxy (a,b)
pTup _ _ = Proxy

pMaybe :: Proxy a -> Proxy (Maybe a)
pMaybe _ = Proxy

pEither :: Proxy a -> Proxy b -> Proxy (Either a b)
pEither _ _ = Proxy

pList :: Proxy a -> Proxy [a]
pList _ = Proxy

pMap :: Proxy a -> Proxy b -> Proxy (Map a b)
pMap _ _ = Proxy

pIntMap :: Proxy a -> Proxy (IntMap a)
pIntMap _ = Proxy

pIntSet :: Proxy (IntSet)
pIntSet = Proxy

pSet :: Proxy a -> Proxy (IntSet)
pSet _ = Proxy

instance (Arbitrary a, Arbitrary b, Ord a) => Arbitrary (Map a b) where
  arbitrary = Map.fromList <$> arbitrary

instance (Arbitrary b) => Arbitrary (IntMap b) where
  arbitrary = IntMap.fromList <$> arbitrary

instance Arbitrary (IntSet) where
  arbitrary = IntSet.fromList <$> arbitrary
