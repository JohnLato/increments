{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wall #-}
module Data.Delta (
  DeltaC (..)
, Changed (..)
, DeltaCnstr
) where

import Data.Delta.Internal

-- | A useful type constraint synonym for writing instances
type DeltaCnstr a = (DeltaC a, Changed (Delta a))
