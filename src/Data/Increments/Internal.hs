{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS -Wall -fno-warn-orphans #-}
module Data.Increments.Internal (
  Incremental (..)
, Changed (..)
, IncrementalCnstr
-- * helpers for creating instances for primitive-ish types
, DPrim (..)
, iprimDiff
, iprimApply
) where

import Control.Arrow                         (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import GHC.Generics

import Data.Beamable
import Data.Beamable.Internal

----------------------------------------
-- int types

instance Incremental Integer where
    type Increment Integer = DPrim Integer
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Int where
    type Increment Int = DPrim Int
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Int8 where
    type Increment Int8 = DPrim Int8
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Int16 where
    type Increment Int16 = DPrim Int16
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Int32 where
    type Increment Int32 = DPrim Int32
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Int64 where
    type Increment Int64 = DPrim Int64
    changes = iprimDiff
    applyChanges = iprimApply

----------------------------------------
-- Word types

instance Incremental Word where
    type Increment Word = DPrim Word
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Word8 where
    type Increment Word8 = DPrim Word8
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Word16 where
    type Increment Word16 = DPrim Word16
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Word32 where
    type Increment Word32 = DPrim Word32
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Word64 where
    type Increment Word64 = DPrim Word64
    changes = iprimDiff
    applyChanges = iprimApply

----------------------------------------
-- floating types

instance Incremental Float where
    type Increment Float = DPrim Float
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Double where
    type Increment Double = DPrim Double
    changes = iprimDiff
    applyChanges = iprimApply

----------------------------------------
-- other basic, non-derived instances

instance Eq x => Incremental [x] where
    type Increment [x] = DPrim [x]
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental Char where
    type Increment Char = DPrim Char
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental B.ByteString where
    type Increment B.ByteString = DPrim B.ByteString
    changes = iprimDiff
    applyChanges = iprimApply

instance Incremental BL.ByteString where
    type Increment BL.ByteString = DPrim BL.ByteString
    changes = iprimDiff
    applyChanges = iprimApply

----------------------------------------
-- derived instances

instance Changed () where
    didChange _ = False

instance Incremental ()
instance Incremental Ordering
instance Incremental Bool
instance (Incremental a, Changed (Increment a)) => Incremental (Maybe a)
instance (Incremental l, Incremental r, Changed (Increment l), Changed (Increment r))
         => Incremental (Either l r)


----------------------------------------
-- tuple instances

instance (Incremental l, Incremental r, Changed (Increment l), Changed (Increment r)) => Incremental (l,r)
instance (Incremental a, Incremental b, Incremental c, Changed (Increment a), Changed (Increment b), Changed (Increment c)) => Incremental (a,b,c)
instance (Incremental a, Incremental b, Incremental c, Incremental d, Changed (Increment a), Changed (Increment b), Changed (Increment c), Changed (Increment d)) => Incremental (a,b,c,d)
instance (Incremental a, Incremental b, Incremental c, Incremental d, Incremental e, Changed (Increment a), Changed (Increment b), Changed (Increment c), Changed (Increment d), Changed (Increment e)) => Incremental (a,b,c,d,e)
instance (Incremental a, Incremental b, Incremental c, Incremental d, Incremental e, Incremental f, Changed (Increment a), Changed (Increment b), Changed (Increment c), Changed (Increment d), Changed (Increment e), Changed (Increment f)) => Incremental (a,b,c,d,e,f)
instance (Incremental a, Incremental b, Incremental c, Incremental d, Incremental e, Incremental f, Incremental g, Changed (Increment a), Changed (Increment b), Changed (Increment c), Changed (Increment d), Changed (Increment e), Changed (Increment f), Changed (Increment g)) => Incremental (a,b,c,d,e,f,g)

-- ---------------------------------------------------------------------
-- wrap primitive-ish types in DPrim, and send new values only if there's been
-- a change

data DPrim a =
    DPrim a
  | DPrim_NoChange
  deriving (Eq, Show, Generic)

iprimDiff :: Eq a => a -> a -> DPrim a
iprimDiff a b
    | b == a    = DPrim_NoChange
    | otherwise = DPrim b

iprimApply :: a -> DPrim a -> a
iprimApply _ (DPrim a)        = a
iprimApply a (DPrim_NoChange) = a

instance Beamable a => Beamable (DPrim a)

instance Changed (DPrim a) where
    didChange (DPrim _)      = True
    didChange DPrim_NoChange = False

-- ---------------------------------------------------------------------
-- Main user-visible classes

-- | Determine if a Increment representation contains a real change.  Unchanging
-- changess may be omitted.
class Changed a where
    didChange :: a -> Bool

-- | Calculate differences between data structures.
class Incremental a where
    type Increment a :: *
    -- slightly bogus, this only works because the generic param p is
    -- instantiated to ().  Tough luck if that changes...
    type Increment a = GIncrement (Rep a) ()
    -- | generate the changes between the 'previous' and 'current' data
    changes :: a -> a -> Increment a
    default changes :: (Generic a, GIncremental (Rep a), GChanged (GIncrement (Rep a)), Increment a ~ GIncrement (Rep a) x) => a -> a -> Increment a
    changes a b = gchanges (from a) (from b)

    -- | Apply a changes to a value
    applyChanges :: a -> Increment a -> a
    default applyChanges :: (Generic a, GIncremental (Rep a), GChanged (GIncrement (Rep a)), Increment a ~ GIncrement (Rep a) x) => a -> Increment a -> a
    applyChanges a d_a = to $ gapplyChanges (from a) d_a

-- | A useful type constraint synonym for writing instances
type IncrementalCnstr a = (Incremental a, Changed (Increment a))

-- ---------------------------------------------------------------------
-- proxy tagged types
-- could use Data.Tagged, but those don't have generic instances, and Tagged
-- has the parameters in the wrong order.

newtype P2 a p = P2 a
  deriving (Show, Generic)

instance Beamable a => Beamable (P2 a p)
instance Changed a => Changed (P2 a p) where
    didChange (P2 a)      = didChange a

data PSum a b p =
    PSNeither
  | PSLeft  (GIncrement a p)
  | PSRight (GIncrement b p)
  | TLeft  (a p)
  | TRight (b p)
  deriving (Generic)

instance Changed (PSum a b p) where
    didChange PSNeither = False
    didChange _ = True

data PProd a b p =
    PPNeither
  | PPLeft  (a p)
  | PPRight (b p)
  | PProd   (a p) (b p)
  deriving (Show, Generic)

instance Changed (PProd a b p) where
    didChange PPNeither = False
    didChange _         = True

instance (Beamable (a p), Beamable (b p)) => Beamable (PProd a b p)
instance (Beamable (a p), Beamable (b p), Beamable (GIncrement a p), Beamable (GIncrement b p))
         => Beamable (PSum a b p)

-- ---------------------------------------------------------------------
-- generic version of Changed

class GChanged a where
    g_didChange :: a p -> Bool

instance Changed a => GChanged (P2 a) where
    g_didChange (P2 a)     = didChange a

instance (GChanged (GIncrement a), GChanged (GIncrement b)) => GChanged (PSum a b) where
    g_didChange (PSLeft d)  = g_didChange d
    g_didChange (PSRight d) = g_didChange d
    g_didChange PSNeither   = False
    g_didChange _           = True

instance (GChanged a, GChanged b) => GChanged (PProd a b) where
    g_didChange (PProd a b) = g_didChange a || g_didChange b
    g_didChange (PPLeft a)  = g_didChange a
    g_didChange (PPRight b) = g_didChange b
    g_didChange PPNeither   = False

-- ---------------------------------------------------------------------
-- generic version of Incremental

class (GChanged (GIncrement f)) => GIncremental f where
    type GIncrement f :: * -> *
    gchanges :: f a -> f a -> GIncrement f a
    gapplyChanges :: f a -> GIncrement f a -> f a

instance GIncremental U1 where
    type GIncrement U1 = P2 ()
    gchanges U1 U1 = P2 ()
    gapplyChanges U1 (P2 ()) = U1

instance (GIncremental a, GIncremental b) => GIncremental (a :*: b) where
    type GIncrement (a :*: b) = PProd (GIncrement a) (GIncrement b)
    gchanges (a1 :*: b1) (a2 :*: b2) =
        let d_a = gchanges a1 a2
            d_b = gchanges b1 b2
        in case (g_didChange d_a, g_didChange d_b) of 
            (True, True  ) -> PProd  d_a d_b
            (True, False ) -> PPLeft d_a
            (False, True ) -> PPRight d_b
            (False, False) -> PPNeither
    gapplyChanges (a :*: b) (PProd d_a d_b) = gapplyChanges a d_a :*: gapplyChanges b d_b
    gapplyChanges (a :*: b) (PPLeft d_a)  = gapplyChanges a d_a :*: b
    gapplyChanges (a :*: b) (PPRight d_b) = a :*: gapplyChanges b d_b
    gapplyChanges (a :*: b) (PPNeither)   = a :*: b

instance (GIncremental a, GIncremental b) => GIncremental (a :+: b) where
    type GIncrement (a :+: b) = PSum a b
    gchanges (L1 a) (L1 b) = let d_a = gchanges a b
                           in if g_didChange d_a
                                  then PSLeft d_a
                                  else PSNeither
    gchanges (R1 a) (R1 b) = let d_b = gchanges a b
                           in if g_didChange d_b
                                  then PSRight d_b
                                  else PSNeither
    gchanges (L1 _) (R1 b) = TRight b
    gchanges (R1 _) (L1 b) = TLeft  b
    gapplyChanges (L1 a) (PSLeft  d)
        | g_didChange d = L1 $ gapplyChanges a d
        | otherwise   = L1 a
    gapplyChanges _ (TLeft a)        = L1 a
    gapplyChanges (R1 a) (PSRight d)
        | g_didChange d = R1 $ gapplyChanges a d
        | otherwise   = R1 a
    gapplyChanges _ (TRight a)       = R1 a
    gapplyChanges _ _                = error "Data.Increments: malformed Increment Rep"

newtype GIncrement_K1 a p = GIncrement_K1 (Increment a) deriving Generic

deriving instance (Show (Increment a)) => Show (GIncrement_K1 a p)

instance (Beamable (Increment a)) => Beamable (GIncrement_K1 a p)

instance Changed (Increment a) => Changed (GIncrement_K1 a p) where
    didChange (GIncrement_K1 a) = didChange a

instance Changed (Increment a) => GChanged (GIncrement_K1 a) where
    g_didChange (GIncrement_K1 a) = didChange a
    

instance (Incremental a, Changed (Increment a)) => GIncremental (K1 i a) where
    type GIncrement (K1 i a) = GIncrement_K1 a
    gchanges (K1 a) (K1 b) = GIncrement_K1 $ changes a b
    gapplyChanges (K1 a) (GIncrement_K1 d_a)     = K1 $ a `applyChanges` d_a

-- this instance used for datatypes with single constructor only
instance (GIncremental a, Datatype d, Constructor c) => GIncremental (M1 D d (M1 C c a)) where
    type GIncrement (M1 D d (M1 C c a)) = GIncrement a
    gchanges (M1 (M1 a)) (M1 (M1 b)) = gchanges a b
    gapplyChanges (M1 (M1 a)) d_a
        | g_didChange d_a = M1 (M1 (a `gapplyChanges` d_a))
        | otherwise       = M1 (M1 a)

-- this instance used for  datatypes with multiple constructors
instance (GIncremental a, Constructor c) => GIncremental (M1 C c a) where
    type GIncrement (M1 C c a) = GIncrement a
    gchanges (M1 a) (M1 b) = gchanges a b
    gapplyChanges (M1 a) d_a
        | g_didChange d_a = M1 (a `gapplyChanges` d_a)
        | otherwise       = M1 a

-- this instance is needed to avoid overlapping instances with (M1 D d (M1 C c a))
instance (Datatype d, GIncremental a, GIncremental b) => GIncremental (M1 D d (a :+: b) ) where
    type GIncrement (M1 D d (a :+: b)) = GIncrement (a :+: b)
    gchanges (M1 a) (M1 b) = gchanges a b
    gapplyChanges (M1 a) d_a
        | g_didChange d_a = M1 (a `gapplyChanges` d_a)
        | otherwise       = M1 a

instance (GIncremental a) => GIncremental (M1 S c a) where
    type GIncrement (M1 S c a) = GIncrement a
    gchanges (M1 a) (M1 b)   = gchanges a b
    gapplyChanges (M1 a) d_a
        | g_didChange d_a  = M1 (a `gapplyChanges` d_a)
        | otherwise        = M1 a

instance Beamable (U1 x) where
    beam U1 = beam ()
    unbeam  = first (\() -> U1) . unbeam
    typeSignR l U1 = typeSignR l ()

instance Beamable a => Beamable (K1 i a x) where
    beam (K1 a) = beam a
    unbeam = first K1 . unbeam
    typeSignR l (K1 a) = typeSignR l a

instance Beamable (a x) => Beamable (M1 s c a x) where
    beam (M1 a) = beam a
    unbeam      = first M1 . unbeam
    typeSignR l (M1 a) = typeSignR l a

instance (Beamable (a x), Beamable (b x)) => Beamable ((a :*: b) x) where
    beam (a :*: b) = beam (a,b)
    unbeam = first (uncurry (:*:)) . unbeam
    typeSignR l (a :*: b) = typeSignR l (a,b)

instance (Beamable (a x), Beamable (b x)) => Beamable ((a :+: b) x) where
    beam (L1 a) = beam (Left a :: Either (a x) (b x))
    beam (R1 b) = beam (Right b :: Either (a x) (b x))
    unbeam = first (either L1 R1) . unbeam
    typeSignR l (L1 a)= typeSignR l (Left a  :: Either (a x) (b x))
    typeSignR l (R1 a)= typeSignR l (Right a :: Either (a x) (b x))
