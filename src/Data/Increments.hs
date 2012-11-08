{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wall #-}
module Data.Increments (
  Incremental (..)
, Changed (..)
, IncrementalCnstr
) where

import Data.Increments.Containers  ()
import Data.Increments.Internal

-- | A useful type constraint synonym for writing instances
type IncrementalCnstr a = (Incremental a, Changed (Increment a))
