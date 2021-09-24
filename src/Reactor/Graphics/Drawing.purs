-- | This module provides the means of producing a drawing of the reactor's world.
-- | The drawing is then rendered on canvas. Currently, only grid-based drawings
-- | are supported.

module Reactor.Graphics.Drawing
  ( Drawing
  , DrawingM(..)
  , DrawingF(..)
  , fill
  , mapOver
  , mapOverWithIndex
  , Shape(..)
  , Point
  , Size
  , tile
  ) where

import Prelude

import Color (Color)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Foldable (for_)
import Data.Grid (Grid, enumerate)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Reactor.Graphics.CoordinateSystem (RelativeToGrid, grid, relativeTo)
import Reactor.Internal.Helpers (withJust)

type Point = { x :: Number, y :: Number }
type Size = { width :: Number, height :: Number }

data Shape =
  Rectangle
    (Int -> Point)
    (Int -> { width :: Int, height :: Int })

-- | A 1-square tile on the given point in the grid.
tile :: RelativeToGrid Point -> Shape
tile origin = Rectangle
  (const $ unwrap origin)
  (const { width: 1, height: 1 })

-- | A DSL for constructing drawings. Currently, only filled shapes are supported. Mostly for internal use.
-- | Most of the time, you construct a drawing by calling the different helper functions, like `fill`, instead of
-- | manually constructing a `DrawingF`.
data DrawingF a
  = Filled Color Shape a

derive instance functorDrawingF :: Functor m => Functor DrawingF

-- | A free monad for `DrawingF` that enables you to use `do`-notation when constructing drawings.
-- | Internal implementation detail.
newtype DrawingM a = DrawingM (Free DrawingF a)

derive newtype instance functorDrawingM :: Functor DrawingM
derive newtype instance applyDrawingM :: Apply DrawingM
derive newtype instance applicativeDrawingM :: Applicative DrawingM
derive newtype instance bindDrawingM :: Bind DrawingM
derive newtype instance monadDrawingM :: Monad DrawingM
derive newtype instance semigroupDrawingM :: Semigroup a => Semigroup (DrawingM a)
derive newtype instance monoidDrawingM :: Monoid a => Monoid (DrawingM a)

instance monadRecDrawingM :: MonadRec DrawingM where
  tailRecM k a =
    k a >>= case _ of
      Loop x -> tailRecM k x
      Done y -> pure y

-- | `DrawingM` is usually too general; our drawing-contructing functions don't need to return
-- | anything. Internal implementation detail.
-- |
-- | Usually, constructing a drawing will look something like this.
-- | ```haskell
-- | drawTwoPoints :: CoordinateSystem Point -> CoordinateSystem Point -> Drawing
-- | drawTwoPoints blueXY redXY = do
-- |   fill Color.blue400 $ tile blueXY
-- |   fill Color.red400 $ tile redXY
-- | ```
type Drawing = DrawingM Unit

-- | Fill a shape with a color.
fill :: Color -> Shape -> Drawing
fill color shape = DrawingM $ liftF $ Filled color shape unit

-- | Produce a drawing from a grid. For each tile in the grid, call the supplied function
-- | to obtain its color.
mapOver :: forall a. Grid a -> (a -> Maybe Color) -> Drawing
mapOver g f =
  for_ (enumerate g) $ \(point /\ a) ->
    withJust (f a) \color ->
      fill color $ tile $ point `relativeTo` grid

-- | Produce a drawing from a grid. For each tile in the grid, call the supplied function
-- | to obtain its color. The function receives not only the value of the tile, but also its index.
mapOverWithIndex
  :: forall a
   . Grid a
  -> ({ x :: Int, y :: Int } -> a -> Maybe Color)
  -> Drawing
mapOverWithIndex g f =
  for_ (enumerate g) $ \(point /\ x) ->
    withJust (f point x) \color ->
      fill color $ tile $ point `relativeTo` grid
