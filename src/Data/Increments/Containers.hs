{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Data.Increments.Containers (
  changesSetLike
, applySetLike
, changesMapLike
, applyMapLike
) where

import Data.Increments.Internal

import Data.Beamable
import Data.List             (foldl')
import GHC.Generics

import Data.IntMap           (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet           (IntSet)
import qualified Data.IntSet as IntSet

import Data.Map              (Map)
import qualified Data.Map as Map
import Data.Set              (Set)
import qualified Data.Set as Set

data AddItem key a = AddItem key a  deriving (Eq, Show, Generic)
data RemItem a = RemItem a          deriving (Eq, Show, Generic)

instance (Beamable k, Beamable a) => Beamable (AddItem k a)
instance (Beamable a)             => Beamable (RemItem a)

instance (Ord k, Eq a) => Incremental (Map k a) where
    type Increment (Map k a) = ([AddItem k a],[RemItem k])
    changes      = changesMapLike Map.toList
    applyChanges = applyMapLike Map.insert Map.delete

instance (Eq a) => Incremental (IntMap a) where
    type Increment (IntMap a) = ([AddItem Int a],[RemItem Int])
    changes      = changesMapLike IntMap.toList
    applyChanges = applyMapLike IntMap.insert IntMap.delete

instance Ord a => Incremental (Set a) where
    type Increment (Set a) = ([AddItem () a],[RemItem a])
    changes      = changesSetLike Set.toList Set.difference
    applyChanges = applySetLike Set.insert Set.delete

instance Incremental IntSet where
    type Increment IntSet = ([AddItem () Int],[RemItem Int])
    changes      = changesSetLike IntSet.toList IntSet.difference
    applyChanges = applySetLike IntSet.insert IntSet.delete

instance Changed ([AddItem a b],[RemItem c]) where
    didChange ([],[]) = False
    didChange _       = True

-- TODO: make smart instances that just create a new collection if that would be
-- more efficient.
changesSetLike :: (c -> [a]) -> (c -> c -> c) -> c -> c -> ([AddItem () a],[RemItem a])
changesSetLike toList diffFn prev this =
    let adds = map (AddItem ()) . toList $ diffFn this prev
        rems = map (RemItem)    . toList $ diffFn prev this
    in (adds,rems)

applySetLike :: (a -> c -> c) -> (a -> c -> c) -> c -> ([AddItem () a],[RemItem a]) -> c
applySetLike addFn delFn cnt (adds,rems) =
    let cnt'  = foldl' (\acc (RemItem x) -> delFn x acc) cnt rems
    in foldl' (\acc (AddItem _ x) -> addFn x acc) cnt' adds


changesMapLike :: (Ord k, Eq a) => (c -> [(k,a)]) -> c -> c -> ([AddItem k a],[RemItem k])
changesMapLike toList prev this =
    let proc adds rems p@((prevKey,prevVal):prevs) t@((thisKey,thisVal):these)
          | prevKey < thisKey   = proc adds (RemItem prevKey:rems) prevs t
          | prevKey > thisKey   = proc (AddItem thisKey thisVal:adds) rems p these
          | prevVal /= thisVal  = proc (AddItem thisKey thisVal:adds) rems prevs these
          | otherwise           = proc adds rems prevs these
        proc adds rems prevs [] = (reverse adds, reverse rems ++ map (RemItem . fst) prevs)
        proc adds rems [] these = (reverse adds ++ map (uncurry AddItem) these, reverse rems)
    in proc [] [] (toList prev) (toList this)

applyMapLike :: (k -> a -> c -> c)
             -> (k -> c -> c)
             -> c
             -> ([AddItem k a],[RemItem k])
             -> c
applyMapLike addFn delFn cnt (adds,rems) =
    let cnt'  = foldl' (\acc (RemItem k) -> delFn k acc) cnt rems
    in foldl' (\acc (AddItem k x) -> addFn k x acc) cnt' adds
