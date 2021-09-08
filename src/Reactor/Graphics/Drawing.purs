module Reactor.Graphics.Drawing
  ( Drawing
  , DrawingM(..)
  , DrawingF(..)
  , fill
  , mapOver
  , mapOverWithIndex
  , Shape(..)
  , Size
  , Point
  , cell
  ) where

import Prelude

import Color (Color)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Foldable (for_)
import Data.Grid (Grid, enumerate)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Reactor.Internal.Helpers (withJust)
import Reactor.Graphics.CoordinateSystem (CoordinateSystem(..), grid, wrt)

type Point = { x :: Number, y :: Number }
type Size = { width :: Number, height :: Number }

data Shape = Rectangle (CoordinateSystem Point) (CoordinateSystem Size)

cell :: CoordinateSystem Point -> Shape
cell origin = Rectangle origin (RelativeToGrid { width: 1.0, height: 1.0 })

data DrawingF a
  = Filled Color Shape a

derive instance functorDrawingF :: Functor m => Functor DrawingF

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

type Drawing = DrawingM Unit

fill :: Color -> Shape -> Drawing
fill color shape = DrawingM $ liftF $ Filled color shape unit

mapOver :: forall a. Grid a -> (a -> Maybe Color) -> Drawing
mapOver g f =
  for_ (enumerate g) $ \(point /\ x) ->
    withJust (f x) \color ->
      fill color $ cell $ point `wrt` grid

mapOverWithIndex
  :: forall a
   . Grid a
  -> ({ x :: Int, y :: Int } -> a -> Maybe Color)
  -> Drawing
mapOverWithIndex g f =
  for_ (enumerate g) $ \(point /\ x) ->
    withJust (f point x) \color ->
      fill color $ cell $ point `wrt` grid
