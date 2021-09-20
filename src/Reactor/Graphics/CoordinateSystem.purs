-- | This module provies a wrapper that signalizes whether the wrapped
-- | contents (usually points in space) should be taken relative to the rendering canvas
-- | (i.e. they are in 'pts') or relative to the grid (i.e. they are in 'tiles').

module Reactor.Graphics.CoordinateSystem
  ( CoordinateSystem(..)
  , relativeTo
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
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Heterogeneous.Mapping (class HMap, hmap)

type Point = { x :: Number, y :: Number }

data CoordinateSystem a = RelativeToGrid a | RelativeToCanvas a

derive instance functorCoordinateSystem :: Functor CoordinateSystem

derive instance genericMouseEvent :: Generic (CoordinateSystem a) _
instance showMouseEvent :: Show a => Show (CoordinateSystem a) where
  show = genericShow

-- | Unwrap the wrapped thing and pass it to a function.
withCoords :: forall a b. CoordinateSystem a -> (a -> b) -> b
withCoords (RelativeToGrid x) f = f x
withCoords (RelativeToCanvas x) f = f x

-- | With respect to. Used to wrap things in a coordinate system in a more pleasant way
-- | than what would be possible with the constructors. For example
-- | ```
-- | { x: 1, y: 1 } `relativeTo` grid == RelativeToGrid { x: toNumber 1, y: toNumber 1}
-- | ```
relativeTo :: forall a b. a -> (a -> CoordinateSystem b) -> CoordinateSystem b
relativeTo = (#)

-- | To be used with `relativeTo`, usually `{ some record } `relativeTo` canvas.
canvas :: forall a. a -> CoordinateSystem a
canvas = RelativeToCanvas

-- | To be used with `relativeTo`, usually `{ some record } `relativeTo` grid. Automatically converts
-- | the numbers in `{ some record }` from `Int` to `Number`.
grid :: forall a b. HMap (Int -> Number) a b => a -> CoordinateSystem b
grid = RelativeToGrid <<< hmap (\n -> toNumber n)

-- | A simple helper functions that subtracts 1 from the `y` coordinate.
moveUp
  :: forall cs a r
   . Ring a
  => Functor cs
  => cs { x :: a, y :: a | r }
  -> cs { x :: a, y :: a | r }
moveUp = map \r@{ y } -> r { y = y - one }

-- | A simple helper functions that adds 1 to the `y` coordinate.
moveDown
  :: forall cs a r
   . Ring a
  => Functor cs
  => cs { x :: a, y :: a | r }
  -> cs { x :: a, y :: a | r }
moveDown = map \r@{ y } -> r { y = y + one }

-- | A simple helper functions that subtracts 1 from the `x` coordinate.
moveLeft
  :: forall cs a r
   . Ring a
  => Functor cs
  => cs { x :: a, y :: a | r }
  -> cs { x :: a, y :: a | r }
moveLeft = map \r@{ x } -> r { x = x - one }

-- | A simple helper functions that adds 1 to the `x` coordinate.
moveRight
  :: forall cs a r
   . Ring a
  => Functor cs
  => cs { x :: a, y :: a | r }
  -> cs { x :: a, y :: a | r }
moveRight = map \r@{ x } -> r { x = x + one }

-- | Mostly for internal use.
relativeToGrid
  :: forall a b
   . HMap (Number -> Int) a b
  => Number
  -> CoordinateSystem a
  -> b
relativeToGrid _ (RelativeToGrid x) = hmap (\n -> floor n) x
relativeToGrid tileSize (RelativeToCanvas x) = hmap (\n -> floor (n / tileSize)) x