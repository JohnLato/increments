{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
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
module Data.Delta.Internal (
  DeltaC (..)
, Changed (..)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import GHC.Generics

import Data.Beamable

----------------------------------------
-- int types

instance DeltaC Integer where
    type Delta Integer = DPrim Integer
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Int where
    type Delta Int = DPrim Int
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Int8 where
    type Delta Int8 = DPrim Int8
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Int16 where
    type Delta Int16 = DPrim Int16
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Int32 where
    type Delta Int32 = DPrim Int32
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Int64 where
    type Delta Int64 = DPrim Int64
    delta = dprimDelta
    applyDelta = dprimApply

----------------------------------------
-- Word types

instance DeltaC Word where
    type Delta Word = DPrim Word
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Word8 where
    type Delta Word8 = DPrim Word8
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Word16 where
    type Delta Word16 = DPrim Word16
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Word32 where
    type Delta Word32 = DPrim Word32
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Word64 where
    type Delta Word64 = DPrim Word64
    delta = dprimDelta
    applyDelta = dprimApply

----------------------------------------
-- floating types

instance DeltaC Float where
    type Delta Float = DPrim Float
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Double where
    type Delta Double = DPrim Double
    delta = dprimDelta
    applyDelta = dprimApply

----------------------------------------
-- other basic, non-derived instances

instance Eq x => DeltaC [x] where
    type Delta [x] = DPrim [x]
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC Char where
    type Delta Char = DPrim Char
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC B.ByteString where
    type Delta B.ByteString = DPrim B.ByteString
    delta = dprimDelta
    applyDelta = dprimApply

instance DeltaC BL.ByteString where
    type Delta BL.ByteString = DPrim BL.ByteString
    delta = dprimDelta
    applyDelta = dprimApply

----------------------------------------
-- derived instances

instance DeltaC ()
instance DeltaC Ordering
instance DeltaC Bool
instance (DeltaC a, Changed (Delta a)) => DeltaC (Maybe a)
instance (DeltaC l, DeltaC r, Changed (Delta l), Changed (Delta r))
         => DeltaC (Either l r)


----------------------------------------
-- tuple instances

instance (DeltaC l, DeltaC r, Changed (Delta l), Changed (Delta r)) => DeltaC (l,r)
instance (DeltaC a, DeltaC b, DeltaC c, Changed (Delta a), Changed (Delta b), Changed (Delta c)) => DeltaC (a,b,c)
instance (DeltaC a, DeltaC b, DeltaC c, DeltaC d, Changed (Delta a), Changed (Delta b), Changed (Delta c), Changed (Delta d)) => DeltaC (a,b,c,d)
instance (DeltaC a, DeltaC b, DeltaC c, DeltaC d, DeltaC e, Changed (Delta a), Changed (Delta b), Changed (Delta c), Changed (Delta d), Changed (Delta e)) => DeltaC (a,b,c,d,e)
instance (DeltaC a, DeltaC b, DeltaC c, DeltaC d, DeltaC e, DeltaC f, Changed (Delta a), Changed (Delta b), Changed (Delta c), Changed (Delta d), Changed (Delta e), Changed (Delta f)) => DeltaC (a,b,c,d,e,f)
instance (DeltaC a, DeltaC b, DeltaC c, DeltaC d, DeltaC e, DeltaC f, DeltaC g, Changed (Delta a), Changed (Delta b), Changed (Delta c), Changed (Delta d), Changed (Delta e), Changed (Delta f), Changed (Delta g)) => DeltaC (a,b,c,d,e,f,g)

-- ---------------------------------------------------------------------
-- wrap primitive-ish types in DPrim, and send new values only if there's been
-- a change

data DPrim a =
    DPrim a
  | DPrim_NoChange
  deriving (Eq, Show, Generic)

dprimDelta :: Eq a => a -> a -> DPrim a
dprimDelta a b
    | b == a    = DPrim_NoChange
    | otherwise = DPrim b

dprimApply :: a -> DPrim a -> a
dprimApply _ (DPrim a)        = a
dprimApply a (DPrim_NoChange) = a

instance Beamable a => Beamable (DPrim a)

instance Changed (DPrim a) where
    didChange (DPrim _)      = True
    didChange DPrim_NoChange = False

-- ---------------------------------------------------------------------
-- Main user-visible classes

-- | Determine if a Delta representation contains a real change.  Unchanging
-- deltas may be omitted.
class Changed a where
    didChange :: a -> Bool

-- | Calculate differences between data structures.
class DeltaC a where
    type Delta a :: *
    -- slightly bogus, this only works because the generic param p is
    -- instantiated to ().  Tough luck if that changes...
    type Delta a = GDelta (Rep a) ()
    -- | generate the delta between the 'previous' and 'current' data
    delta :: a -> a -> Delta a
    default delta :: (Generic a, GDeltaC (Rep a), GChanged (GDelta (Rep a)), Delta a ~ GDelta (Rep a) x) => a -> a -> Delta a
    delta a b = gdelta (from a) (from b)

    -- | Apply a delta to a value
    applyDelta :: a -> Delta a -> a
    default applyDelta :: (Generic a, GDeltaC (Rep a), GChanged (GDelta (Rep a)), Delta a ~ GDelta (Rep a) x) => a -> Delta a -> a
    applyDelta a d_a = to $ gapplyDelta (from a) d_a


-- ---------------------------------------------------------------------
-- proxy tagged types
-- could use Data.Tagged, but those don't have generic instances, and Tagged
-- has the parameters in the wrong order.

data Proxy p = Proxy deriving (Show, Generic)

instance Beamable (Proxy p)
instance Changed (Proxy p) where
    didChange _ = False

newtype P2 a p = P2 a
  deriving (Show, Generic)

instance Beamable a => Beamable (P2 a p)
instance Changed a => Changed (P2 a p) where
    didChange (P2 a)      = didChange a

data PSum a b delta_a delta_b p =
    PSNeither
  | PSLeft  (delta_a p)
  | PSRight (delta_b p)
  | TLeft  (a p)
  | TRight (b p)
  deriving (Show, Generic)

instance Changed (PSum a b d_a d_b p) where
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
instance (Beamable (a p), Beamable (b p), Beamable (d_a p), Beamable (d_b p))
         => Beamable (PSum a b d_a d_b p)

-- ---------------------------------------------------------------------
-- generic version of Changed

class GChanged a where
    g_didChange :: a p -> Bool

instance GChanged Proxy where
    g_didChange _ = False

instance Changed a => GChanged (P2 a) where
    g_didChange (P2 a)     = didChange a

instance (GChanged d_a, GChanged d_b) => GChanged (PSum a b d_a d_b) where
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
-- generic version of DeltaC

class (GChanged (GDelta f)) => GDeltaC f where
    type GDelta f :: * -> *
    gdelta :: f a -> f a -> GDelta f a
    gapplyDelta :: f a -> GDelta f a -> f a

instance GDeltaC U1 where
    type GDelta U1 = Proxy
    gdelta U1 U1 = Proxy
    gapplyDelta U1 Proxy = U1

instance (GDeltaC a, GDeltaC b) => GDeltaC (a :*: b) where
    type GDelta (a :*: b) = PProd (GDelta a) (GDelta b)
    gdelta (a1 :*: b1) (a2 :*: b2) =
        let d_a = gdelta a1 a2
            d_b = gdelta b1 b2
        in case (g_didChange d_a, g_didChange d_b) of 
            (True, True  ) -> PProd  d_a d_b
            (True, False ) -> PPLeft d_a
            (False, True ) -> PPRight d_b
            (False, False) -> PPNeither
    gapplyDelta (a :*: b) (PProd d_a d_b) = gapplyDelta a d_a :*: gapplyDelta b d_b
    gapplyDelta (a :*: b) (PPLeft d_a)  = gapplyDelta a d_a :*: b
    gapplyDelta (a :*: b) (PPRight d_b) = a :*: gapplyDelta b d_b
    gapplyDelta (a :*: b) (PPNeither)   = a :*: b

instance (GDeltaC a, GDeltaC b) => GDeltaC (a :+: b) where
    type GDelta (a :+: b) = PSum a b (GDelta a) (GDelta b)
    gdelta (L1 a) (L1 b) = let d_a = gdelta a b
                           in if g_didChange d_a
                                  then PSLeft d_a
                                  else PSNeither
    gdelta (R1 a) (R1 b) = let d_b = gdelta a b
                           in if g_didChange d_b
                                  then PSRight d_b
                                  else PSNeither
    gdelta (L1 _) (R1 b) = TRight b
    gdelta (R1 _) (L1 b) = TLeft  b
    gapplyDelta (L1 a) (PSLeft  d)
        | g_didChange d = L1 $ gapplyDelta a d
        | otherwise   = L1 a
    gapplyDelta _ (TLeft a)        = L1 a
    gapplyDelta (R1 a) (PSRight d)
        | g_didChange d = R1 $ gapplyDelta a d
        | otherwise   = R1 a
    gapplyDelta _ (TRight a)       = R1 a
    gapplyDelta _ _                = error "Data.Delta: malformed delta Rep"

instance (DeltaC a, Changed (Delta a)) => GDeltaC (K1 i a) where
    type GDelta (K1 i a) = P2 (Delta a)
    gdelta (K1 a) (K1 b) = P2 $ delta a b
    gapplyDelta (K1 a) (P2 d_a)     = K1 $ a `applyDelta` d_a

-- this instance used for datatypes with single constructor only
instance (GDeltaC a, Datatype d, Constructor c) => GDeltaC (M1 D d (M1 C c a)) where
    type GDelta (M1 D d (M1 C c a)) = GDelta a
    gdelta (M1 (M1 a)) (M1 (M1 b)) = gdelta a b
    gapplyDelta (M1 (M1 a)) d_a
        | g_didChange d_a = M1 (M1 (a `gapplyDelta` d_a))
        | otherwise       = M1 (M1 a)

-- this instance used for  datatypes with multiple constructors
instance (GDeltaC a, Constructor c) => GDeltaC (M1 C c a) where
    type GDelta (M1 C c a) = GDelta a
    gdelta (M1 a) (M1 b) = gdelta a b
    gapplyDelta (M1 a) d_a
        | g_didChange d_a = M1 (a `gapplyDelta` d_a)
        | otherwise       = M1 a

-- this instance is needed to avoid overlapping instances with (M1 D d (M1 C c a))
instance (Datatype d, GDeltaC a, GDeltaC b) => GDeltaC (M1 D d (a :+: b) ) where
    type GDelta (M1 D d (a :+: b)) = GDelta (a :+: b)
    gdelta (M1 a) (M1 b) = gdelta a b
    gapplyDelta (M1 a) d_a
        | g_didChange d_a = M1 (a `gapplyDelta` d_a)
        | otherwise       = M1 a

instance (GDeltaC a) => GDeltaC (M1 S c a) where
    type GDelta (M1 S c a) = GDelta a
    gdelta (M1 a) (M1 b)   = gdelta a b
    gapplyDelta (M1 a) d_a
        | g_didChange d_a  = M1 (a `gapplyDelta` d_a)
        | otherwise        = M1 a
