{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Data.Increments.Containers (
  MapLikeIncrement
, SetLikeIncrement
, changesSetLike
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

data AddItem k a = AddItem k a             deriving (Eq, Show, Generic)
data RemItem a   = RemItem a               deriving (Eq, Show, Generic)
data ModItem k a = ModItem k (Increment a) deriving (Generic)

-- | The 'Increment' of a map-like (key-value) container.
type MapLikeIncrement k a = ([AddItem k a],[RemItem k], [ModItem k a])

-- | The 'Increment' of a set.
type SetLikeIncrement a = ([AddItem () a],[RemItem a])

deriving instance (Eq (Increment a), Eq key) => Eq (ModItem key a)
deriving instance (Show (Increment a), Show key) => Show (ModItem key a)

instance (Beamable k, Beamable a) => Beamable (AddItem k a)
instance (Beamable a)             => Beamable (RemItem a)
instance (Beamable k, Beamable (Increment a)) => Beamable (ModItem k a)

instance (Ord k, IncrementalCnstr a) => Incremental (Map k a) where
    type Increment (Map k a) = MapLikeIncrement k a
    changes      = changesMapLike Map.toList
    applyChanges = applyMapLike Map.insert Map.delete (\k diff -> Map.update (Just . (`applyChanges` diff)) k)

instance (IncrementalCnstr a) => Incremental (IntMap a) where
    type Increment (IntMap a) = MapLikeIncrement Int a
    changes      = changesMapLike IntMap.toList
    applyChanges = applyMapLike IntMap.insert IntMap.delete (\k diff -> IntMap.update (Just . (`applyChanges` diff)) k)

instance Ord a => Incremental (Set a) where
    type Increment (Set a) = SetLikeIncrement a
    changes      = changesSetLike Set.toList Set.difference
    applyChanges = applySetLike Set.insert Set.delete

instance Incremental IntSet where
    type Increment IntSet = SetLikeIncrement Int
    changes      = changesSetLike IntSet.toList IntSet.difference
    applyChanges = applySetLike IntSet.insert IntSet.delete

instance Changed ([AddItem a b],[RemItem c]) where
    didChange ([],[]) = False
    didChange _       = True

instance Changed (Increment e) => Changed ([AddItem a b],[RemItem c], [ModItem d e]) where
    didChange ([],[],mods) = any (\(ModItem _ diff) -> didChange diff) mods
    didChange _          = True

-- TODO: make smart instances that just create a new collection if that would be
-- more efficient.

-- | a generic 'changes' function, useful for defining instances for sets.
changesSetLike :: (c -> [a]) -> (c -> c -> c) -> c -> c -> SetLikeIncrement a
changesSetLike toList diffFn prev this =
    let adds = map (AddItem ()) . toList $ diffFn this prev
        rems = map (RemItem)    . toList $ diffFn prev this
    in (adds,rems)

-- | a generic 'applyChanges' function, useful for defining instances for sets.
applySetLike :: (a -> c -> c) -> (a -> c -> c) -> c -> SetLikeIncrement a -> c
applySetLike addFn delFn cnt (adds,rems) =
    let cnt'  = foldl' (\acc (RemItem x) -> delFn x acc) cnt rems
    in foldl' (\acc (AddItem _ x) -> addFn x acc) cnt' adds


-- | a generic 'changes' function, useful for defining instances for maps.
changesMapLike :: (Ord k, IncrementalCnstr a) => (c -> [(k,a)]) -> c -> c -> MapLikeIncrement k a
changesMapLike toList prev this =
    let proc adds rems mods p@((prevKey,prevVal):prevs) t@((thisKey,thisVal):these)
          | prevKey < thisKey   = proc adds (RemItem prevKey:rems) mods prevs t
          | prevKey > thisKey   = proc (AddItem thisKey thisVal:adds) rems mods p these
          | otherwise           = let diff = changes prevVal thisVal
                                  in if didChange diff
                                      then proc adds rems (ModItem thisKey (changes prevVal thisVal):mods) prevs these
                                      else proc adds rems mods prevs these
        proc adds rems mods prevs [] = (reverse adds, reverse rems ++ map (RemItem . fst) prevs, reverse mods)
        proc adds rems mods [] these = (reverse adds ++ map (uncurry AddItem) these, reverse rems, reverse mods)
    in proc [] [] [] (toList prev) (toList this)

-- | a generic 'applyChanges' function, useful for defining instances for maps.
applyMapLike :: Incremental a
             => (k -> a -> c -> c)
             -> (k -> c -> c)
             -> (k -> Increment a -> c -> c)
             -> c
             -> MapLikeIncrement k a
             -> c
applyMapLike addFn delFn modFn cnt (adds,rems,mods) =
    let cntPruned = foldl' (\acc (RemItem k) -> delFn k acc) cnt rems
        cntAdded  = foldl' (\acc (AddItem k x) -> addFn k x acc) cntPruned adds
    in  foldl' (\acc (ModItem k diff) -> modFn k diff acc) cntAdded mods
