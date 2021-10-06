-- | This module provides the means of producing a drawing of the reactor's world.
-- | The drawing is then rendered on canvas. Currently, only grid-based drawings
-- | are supported.

module Reactor.Graphics.Drawing
  ( Drawing
  , DrawingM(..)
  , DrawingF(..)
  , fill
  , drawGrid
  , drawGridWithIndex
  , Shape(..)
  , tile
  ) where

import Prelude

import Color (Color)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Foldable (for_)
import Data.Grid (Grid, enumerate, Coordinates)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Reactor.Internal.Helpers (withJust)

data Shape =
  Rectangle
    (Int -> Coordinates)
    (Int -> { width :: Int, height :: Int })

-- | A 1-square tile on the given point in the grid.
tile :: Coordinates -> Shape
tile origin = Rectangle
  (const origin)
  (const { width: 1, height: 1 })

-- | A DSL for constructing drawings. Currently, only filled shapes are supported. `DrawingF` is for internal use.
-- | You should construct a drawing by calling the different helper functions, like `fill`, instead of
-- | manually calling the `DrawingF` data contructors.
data DrawingF a
  = Filled Color Shape a

derive instance functorDrawingF :: Functor m => Functor DrawingF

-- | A free monad for `DrawingF` that enables you to use `do`-notation when constructing drawings.
-- | Just an internal implementation detail.
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

-- | Given a shape (e.g. a rectangle), draw it and fill it with the provided color.
fill :: Color -> Shape -> Drawing
fill color shape = DrawingM $ liftF $ Filled color shape unit

-- | This function produces a drawing from a grid. For each cell in the grid, it calls the supplied
-- | `(a -> Maybe Color)` function to obtain its color.
-- | The cell is then drawn it using the `fill` and `tile` functions.
drawGrid :: forall a. Grid a -> (a -> Maybe Color) -> Drawing
drawGrid grid getTileColor = drawGridWithIndex grid (const getTileColor)

-- | A function almost identical to `drawGrid`, only the function matching colors to grid cells
-- | is provided with an index of the cell in addition to the value of the cell.
drawGridWithIndex :: forall a. Grid a -> (Coordinates -> a -> Maybe Color) -> Drawing
drawGridWithIndex grid getTileColor =
  for_ (enumerate grid) $ \(point /\ t) ->
    withJust (getTileColor point t) \color ->
      fill color $ tile point
