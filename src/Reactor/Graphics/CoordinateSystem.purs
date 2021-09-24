-- | This module provides wrappers that signalize whether the
-- | content (usually points in space) should be taken relative to the rendering canvas
-- | (i.e. they are in 'pts') or relative to the grid (i.e. they are in 'tiles').

module Reactor.Graphics.CoordinateSystem
  ( RelativeToGrid(..)
  , RelativeToCanvas(..)
  , bound
  , class ToGridCoords
  , relativeTo
  , canvas
  , grid
  , convertToGrid
  , moveUp
  , moveDown
  , moveLeft
  , moveRight
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (floor, toNumber)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Heterogeneous.Mapping (class HMap, hmap)

type Point = { x :: Number, y :: Number }

newtype RelativeToGrid a = RelativeToGrid a

derive instance functorRelativeToGrid :: Functor (RelativeToGrid)
derive instance genericRelativeToGrid :: Generic (RelativeToGrid a) _
instance showRelativeToGrid :: Show a => Show (RelativeToGrid a) where
  show = genericShow

instance newtypeRelativeToGrid :: Newtype (RelativeToGrid a) a

newtype RelativeToCanvas a = RelativeToCanvas a

derive instance functorRelativeToCanvas :: Functor (RelativeToCanvas)
derive instance genericRelativeToCanvas :: Generic (RelativeToCanvas a) _
instance showRelativeToCanvas :: Show a => Show (RelativeToCanvas a) where
  show = genericShow

instance newtypeRelativeToCanvas :: Newtype (RelativeToCanvas a) a

class ToCanvasCoords coords where
  relativeToCanvas
    :: forall a b
     . HMap (Number -> Number) a b
    => Int
    -> coords a
    -> RelativeToCanvas b

class ToGridCoords coords where
  convertToGrid
    :: forall a b
     . HMap (Number -> Number) a b
    => Int
    -> coords a
    -> RelativeToGrid b

  bound
    :: Int
    -> Int
    -> coords Point
    -> coords Point

clip :: forall a. Semiring a => Ord a => a -> a -> a
clip n b = max (min n b) zero

instance toGridCoordsRelativeToCanvas :: ToGridCoords RelativeToCanvas where
  convertToGrid tileSize (RelativeToCanvas x) =
    RelativeToGrid $ hmap (toNumber <<< floor <<< (_ / toNumber tileSize)) x
  bound w h (RelativeToCanvas { x, y }) =
    RelativeToCanvas { x: clip x (toNumber w - one), y: clip y (toNumber h - one) }

instance toGridCoordsRelativeToGrid :: ToGridCoords RelativeToGrid where
  convertToGrid
    :: forall a b
     . HMap (Number -> Number) a b
    => Int
    -> RelativeToGrid a
    -> RelativeToGrid b
  convertToGrid _ (RelativeToGrid x) =
    RelativeToGrid $ hmap (identity :: Number -> Number) x
  bound w h (RelativeToGrid { x, y }) =
    RelativeToGrid { x: clip x (toNumber w - one), y: clip y (toNumber h - one) }

instance toCanvasCoordsRelativeToCanvas :: ToCanvasCoords RelativeToGrid where
  relativeToCanvas tileSize (RelativeToGrid x) =
    RelativeToCanvas $ hmap (_ * toNumber tileSize) x

-- | Used to wrap things in a coordinate system in a more pleasant way
-- | than what would be possible with the constructors. For example
-- | ```
-- | { x: 1, y: 1 } `relativeTo` grid == RelativeToGrid { x: toNumber 1, y: toNumber 1}
-- | ```
relativeTo :: forall a b. a -> (a -> b) -> b
relativeTo = (#)

-- | To be used with `relativeTo`, usually `{ some record } `relativeTo` canvas.
canvas :: forall a. a -> RelativeToCanvas a
canvas = RelativeToCanvas

-- | To be used with `relativeTo`, usually `{ some record } `relativeTo` grid. Automatically converts
-- | the numbers in `{ some record }` from `Int` to `Number`.
grid :: forall a b. HMap (Int -> Number) a b => a -> RelativeToGrid b
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
