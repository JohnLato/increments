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
) where

import GHC.Generics

import Data.Beamable
import Data.Maybe (isJust)

instance DeltaC Bool where

instance DeltaC () where
    type Delta () = ()
    delta () () = ()
    applyDelta () () = ()

newtype D_Int = D_Int (Maybe Int) deriving (Show, Generic)

instance Beamable D_Int

instance DeltaC Int where
    type Delta Int = D_Int
    delta a b | b == a = D_Int Nothing
              | otherwise = D_Int (Just b)
    applyDelta _ (D_Int (Just d)) = d
    applyDelta a (D_Int Nothing)  = a

instance DeltaC (Proxy p) where
    type Delta (Proxy p) = ()
    delta _ _ = ()
    applyDelta a _ = a

instance (DeltaC a, Changed (Delta a)) => DeltaC (Maybe a) where

instance (DeltaC l, DeltaC r, Changed (Delta l), Changed (Delta r)) => DeltaC (Either l r) where

-- class (Changed (Delta a)) => DeltaC a where
class DeltaC a where
    type Delta a :: *
    -- slightly bogus, this only works because the generic param p is
    -- instantiated to ().  Tough luck if that changes...
    type Delta a = GDelta (Rep a) ()
    -- | generate the delta between two values
    delta :: a -> a -> Delta a
    default delta :: (Generic a, GDeltaC (Rep a), GChanged (GDelta (Rep a)), Delta a ~ GDelta (Rep a) x) => a -> a -> Delta a
    delta a b = gdelta (from a) (from b)

    applyDelta :: a -> Delta a -> a
    default applyDelta :: (Generic a, GDeltaC (Rep a), GChanged (GDelta (Rep a)), Delta a ~ GDelta (Rep a) x) => a -> Delta a -> a
    applyDelta a d_a = to $ gapplyDelta (from a) d_a

instance Changed () where
    didChange _ = False

instance Changed D_Int where
    didChange (D_Int (Just _)) = True
    didChange (D_Int Nothing)  = False

class (GChanged (GDelta f)) => GDeltaC f where
    type GDelta f :: * -> *
    gdelta :: f a -> f a -> GDelta f a
    gapplyDelta :: f a -> GDelta f a -> f a

data Proxy p = Proxy deriving (Show, Generic)

instance Beamable (Proxy p)
instance Changed (Proxy p) where
    didChange _ = False

newtype P2 a p = P2 (Maybe a) deriving (Show, Generic)

instance Beamable a => Beamable (P2 a p)
instance Changed a => Changed (P2 a p) where
    didChange (P2 (Just a)) = didChange a
    didChange (P2 Nothing)  = False

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

class Changed a where
    didChange :: a -> Bool

class GChanged a where
    g_didChange :: a p -> Bool

instance GChanged Proxy where
    g_didChange _ = False

instance GChanged (P2 a) where
    g_didChange (P2 v) = isJust v

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

instance (Beamable (a p), Beamable (b p), Beamable (d_a p), Beamable (d_b p)) => Beamable (PSum a b d_a d_b p)

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
    gdelta (K1 a) (K1 b) = case delta a b of
                              d | didChange d -> P2 . Just $ d
                                | otherwise   -> P2 Nothing
    gapplyDelta (K1 a) (P2 (Just d_a)) = K1 $ a `applyDelta` d_a 
    gapplyDelta (K1 a) (P2 Nothing)    = K1 a

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
