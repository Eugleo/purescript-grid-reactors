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
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafePartial)
import Reactor.Events (DefaultBehavior(..))
import Reactor.Graphics.Drawing (Drawing, DrawingF(..), DrawingM(..), Shape(..))
import Reactor.Internal.Types (Cell(..), State)
import Reactor.Internal.Widget (Ordering(..), Widget(..))
import Reactor.Reaction (ReactionF(..), ReactionM(..))
import Reactor.Reaction as Reactor
import Type.Proxy (Proxy(..))

evalAction
  :: forall world m a
   . MonadEffect m
  => { width :: Int, tileSize :: Int, height :: Int }
  -> StateId (State m world)
  -> Reactor.ReactionM world a
  -> HookM m DefaultBehavior

evalAction { width, tileSize, height } stateId (ReactionM action) =
  map snd $ runStateT (foldFree (go (Proxy :: Proxy world)) action) Prevent

  where
  go
    :: forall f b
     . MonadEffect f
    => Proxy world
    -> ReactionF world b
    -> StateT DefaultBehavior (HookM f) b
  go _ (Modify modifyWorld cc) = do
    newState <- lift $ Hooks.modify stateId \s -> s { world = modifyWorld s.world }
    pure $ cc newState.world
  go _ (Dimensions cc) = do
    pure $ cc { width, tileSize, height }
  go _ (Lift eff) = liftEffect eff
  go _ (Widget uid ordering w a) = do
    insertWidget (Proxy :: Proxy world) uid ordering w
    pure a
  go _ (RemoveWidget uid a) = do
    _ <- lift $ Hooks.modify stateId \s ->
      s { widgets = Array.filter (different uid) s.widgets }
    pure a
  go _ (ExecuteDefaultBehavior a) = put Execute >>= const (pure a)

  same uid = (_ == uid) <<< fst
  different uid = (_ /= uid) <<< fst

  insertWidget
    :: forall f
     . MonadEffect f
    => Proxy world
    -> String
    -> Ordering
    -> Widget
    -> StateT DefaultBehavior (HookM f) Unit
  insertWidget _ uid ordering w = do
    state <- lift $ Hooks.get stateId
    case Array.findIndex (same uid) state.widgets of
      Nothing -> lift $ Hooks.put stateId $
        state { widgets = into ordering (uid /\ w) state.widgets }
      Just i -> lift $ Hooks.put stateId $
        state
          { widgets =
              unsafePartial $ fromJust $ Array.updateAt i (uid /\ w) state.widgets
          }

  into :: forall a. Ordering -> a -> Array a -> Array a
  into First = Array.cons
  into Last = flip Array.snoc

renderDrawing :: Int -> { width :: Int, height :: Int } -> Drawing -> Grid Cell
renderDrawing tileSize g (DrawingM drawing) = Grid array g
  where
  array = STArray.run do
    grid <- STArray.unsafeThaw $ Array.replicate (g.width * g.height) EmptyCell
    foldFree (go grid) drawing
    pure grid

  go :: forall a r. STArray r Cell -> (DrawingF a) -> ST r a
  go grid (Filled color shape cc) = do
    case shape of
      Rectangle getOrigin getSize -> do
        let
          { x, y } = getOrigin tileSize
          { width, height } = getSize tileSize

        for x (x + width) \i ->
          for y (y + height) \j ->
            STArray.poke (j * g.width + i) (Colored color) grid
        pure cc