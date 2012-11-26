{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wall #-}

-- | Calculate incremental changes of data structures.
-- 
-- The 'Incremental' class provides a set of functions that work like the unix
-- utilities 'diff' and 'patch'.  'changes' generates an incremental diff between
-- two data values.  The incremental diff can then be applied by the
-- 'applyChanges' function.
--
-- The primary intention of this library is to support efficient serialization.
-- As such, default 'Increment' types are automatically provided with
-- 'Beamable' instances.
--
-- > {-# LANGUAGE DeriveGenerics #-}
-- > {-# LANGUAGE ConstraintKinds #-}
-- > {-# LANGUAGE FlexibleContexts #-}
-- > {-# LANGUAGE UndecidableInstances #-}
-- >
-- > import GHC.Generics
-- > import Data.Beamable
-- > import Data.ByteString as B
-- >
-- > data Foo a b = Foo Int (Maybe a) b deriving (Generic, Eq, Show)
-- >
-- > -- If a 'Generic' instance is available, the default definition is
-- > -- fine, after adding some constraints.
-- > instance (IncrementalCnstr a, IncrementalCnstr b) => Incremental (Foo a b)
-- >
-- > -- generate some test data
-- > foo1 = Foo 1 Nothing "foo1"
-- > foo2 = Foo 1 (Just "foo2") "foo1"
-- >
-- > -- the 'changes' function calculates an incremental changeset from
-- > -- 'foo1' to 'foo2'
-- > diff = changes foo1 foo2
-- >
-- > -- 'applyChanges' applies the changes in an incremental patch to some data
-- > -- applyChanges foo1 diff == foo2
-- > -- True
-- >
-- > -- incremental changes can be smaller (sometimes significantly smaller)
-- > -- than the data source
-- > -- B.length $ encode diff
-- > -- 8
-- > -- B.length $ encode foo2
-- > -- 12
-- 
-- Incremental changes are not in general commutative or optional, and
-- it can be an error to apply a change to a data structure that doesn't match
-- the originating structure.  For example:
--
-- > *Data.Increments> let diff = changes (Left 1) (Left 2 :: Either Int Char)
-- > *Data.Increments> applyChanges (Right 'a') diff
-- > *** Exception: Data.Increments: malformed Increment Rep
--
module Data.Increments (
  Incremental (..)
, Changed (..)
, IncrementalCnstr
) where

import Data.Increments.Containers  ()
import Data.Increments.Internal
