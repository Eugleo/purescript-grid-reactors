-- | This module provies a wrapper that signalizes whether the wrapped
-- | contents (usually points in space) should be taken relative to the rendering canvas
-- | (i.e. they are in 'pts') or relative to the grid (i.e. they are in 'tiles').

module Reactor.Graphics.CoordinateSystem
  ( CoordinateSystem(..)
  , wrt
  , canvas
  , grid
  , relativeToGrid
  , withCoords
  , Point(..)
  , moveUp
  , moveDown
  , moveLeft
  , moveRight
  ) where

import Prelude

import Data.Int (floor, toNumber)
import Heterogeneous.Mapping (class HMap, hmap)

type Point = { x :: Number, y :: Number }

data CoordinateSystem a = RelativeToGrid a | RelativeToCanvas a

derive instance functorCoordinateSystem :: Functor CoordinateSystem

-- | Unwrap the wrapped thing and pass it to a function.
withCoords :: forall a b. CoordinateSystem a -> (a -> b) -> b
withCoords (RelativeToGrid x) f = f x
withCoords (RelativeToCanvas x) f = f x

-- | With respect to. Used to wrap things in a coordinate system in a more pleasant way
-- | than what would be possible with the constructors. For example
-- | ```
-- | { x: 1, y: 1 } `wrt` grid == RelativeToGrid { x: toNumber 1, y: toNumber 1}
-- | ```
wrt :: forall a b. a -> (a -> CoordinateSystem b) -> CoordinateSystem b
wrt = (#)

-- | To be used with `wrt`, usually `{ some record } `wrt` canvas.
canvas :: forall a. a -> CoordinateSystem a
canvas = RelativeToCanvas

-- | To be used with `wrt`, usually `{ some record } `wrt` grid. Automatically converts
-- | the numbers in `{ some record }` from `Int` to `Number`.
grid :: forall a b. HMap (Int -> Number) a b => a -> CoordinateSystem b
grid = RelativeToGrid <<< hmap (\n -> toNumber n)

-- | A simple helper functions that subtracts 1 from the `y` coordinate.
moveUp :: CoordinateSystem Point -> CoordinateSystem Point
moveUp = map (\{ x, y } -> { x, y: y - 1.0 })

-- | A simple helper functions that adds 1 to the `y` coordinate.
moveDown :: CoordinateSystem Point -> CoordinateSystem Point
moveDown = map (\{ x, y } -> { x, y: y + 1.0 })

-- | A simple helper functions that subtracts 1 from the `x` coordinate.
moveLeft :: CoordinateSystem Point -> CoordinateSystem Point
moveLeft = map (\{ x, y } -> { x: x - 1.0, y })

-- | A simple helper functions that adds 1 to the `x` coordinate.
moveRight :: CoordinateSystem Point -> CoordinateSystem Point
moveRight = map (\{ x, y } -> { x: x + 1.0, y })

-- | Mostly for internal use.
relativeToGrid
  :: forall a b
   . HMap (Number -> Int) a b
  => Number
  -> CoordinateSystem a
  -> b
relativeToGrid _ (RelativeToGrid x) = hmap (\n -> floor n) x
relativeToGrid tileSize (RelativeToCanvas x) = hmap (\n -> floor (n / tileSize)) x