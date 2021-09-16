module Reactor.Internal.Eval (evalAction, renderDrawing) where

import Prelude

import Control.Monad.Free (foldFree)
import Control.Monad.ST (ST, for)
import Control.Monad.State (StateT, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Grid (Grid(..))
import Data.Int (toNumber)
import Data.Tuple (snd)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Reactor.Action (ActionF(..))
import Reactor.Action as Reactor
import Reactor.Events (DefaultBehavior(..))
import Reactor.Graphics.CoordinateSystem (CoordinateSystem(..), relativeToGrid, Point)
import Reactor.Graphics.Drawing (Drawing, DrawingF(..), DrawingM(..), Shape(..))
import Reactor.Internal.Types (Cell(..), State)
import Type.Proxy (Proxy(..))

evalAction
  :: forall world m a
   . MonadEffect m
  => { width :: Int, tileSize :: Int, height :: Int }
  -> StateId (State m world)
  -> Reactor.Action m world a
  -> HookM m DefaultBehavior
evalAction { width, tileSize, height } stateId (Reactor.Action action) =
  map snd $ runStateT (foldFree (go (Proxy :: Proxy world)) action) Prevent

  where
  go
    :: forall f b
     . MonadEffect f
    => Proxy world
    -> Reactor.ActionF f world b
    -> StateT DefaultBehavior (HookM f) b
  go _ (RandomNumber min max cc) = do
    n <- liftEffect (randomInt min max)
    pure $ cc n
  go _ (Modify modifyWorld cc) = do
    newState <- lift $ Hooks.modify stateId \s -> s { world = modifyWorld s.world }
    pure $ cc newState.world
  go _ (Utilities cc) = do
    let
      w = toNumber width
      h = toNumber height
      cs = toNumber tileSize
      clip n b = max (min n b) 0.0

      bound :: CoordinateSystem Point -> CoordinateSystem Point
      bound = case _ of
        RelativeToCanvas { x, y } ->
          RelativeToCanvas { x: clip x (cs * w - 0.01), y: clip y (cs * h - 0.01) }
        RelativeToGrid { x, y } ->
          RelativeToGrid { x: clip x (w - 1.0), y: clip y (h - 1.0) }
    pure $ cc { width, tileSize, height, bound }
  go _ (Lift ma) = lift $ lift ma
  go _ (ExecuteDefaultBehavior a) = put Execute >>= const (pure a)

renderDrawing :: Number -> { width :: Int, height :: Int } -> Drawing -> Grid Cell
renderDrawing tileSize g (DrawingM drawing) = Grid array g
  where
  array = STArray.run do
    grid <- STArray.new
    _ <- flip STArray.pushAll grid $ Array.replicate (g.width * g.height) EmptyCell
    foldFree (go grid) drawing
    pure grid

  go :: forall a r. STArray r Cell -> (DrawingF a) -> ST r a
  go grid (Filled color shape cc) = do
    case shape of
      Rectangle origin size -> do
        let
          { x, y } = relativeToGrid tileSize origin
          { width, height } = relativeToGrid tileSize size

        for x (x + width) \i ->
          for y (y + height) \j ->
            STArray.poke (j * g.width + i) (Colored color) grid
        pure cc