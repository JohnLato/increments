{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wall #-}
module Data.Increments (
  Incremental (..)
, Changed (..)
, IncrementalCnstr
) where

import Data.Increments.Containers  ()
import Data.Increments.Internal
