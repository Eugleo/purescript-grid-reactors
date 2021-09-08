module Reactor.Graphics.CoordinateSystem
  ( CoordinateSystem(..)
  , wrt
  , canvas
  , grid
  , relativeToGrid
  ) where

import Prelude

import Data.Int (floor, toNumber)
import Heterogeneous.Mapping (class HMap, hmap)

data CoordinateSystem a = RelativeToGrid a | RelativeToCanvas a
derive instance functorCoordinateSystem :: Functor CoordinateSystem

wrt :: forall a b. a -> (a -> CoordinateSystem b) -> CoordinateSystem b
wrt = (#)

canvas :: forall a. a -> CoordinateSystem a
canvas = RelativeToCanvas

grid :: forall a b. HMap (Int -> Number) a b => a -> CoordinateSystem b
grid = RelativeToGrid <<< hmap (\n -> toNumber n)

relativeToGrid
  :: forall a b
   . HMap (Number -> Int) a b
  => Number
  -> CoordinateSystem a
  -> b
relativeToGrid _ (RelativeToGrid x) = hmap (\n -> floor n) x
relativeToGrid cellSize (RelativeToCanvas x) = hmap (\n -> floor (n / cellSize)) x