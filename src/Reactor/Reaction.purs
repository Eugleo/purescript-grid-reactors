module Reactor.Reaction
  ( Reaction(..)
  , dimensions
  , modifyW
  , modifyW_
  , updateW
  , updateW_
  , getW
  , executeDefaultBehavior
  , ReactionM(..)
  , ReactionF(..)
  , Dimensions
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row (class Union, class Nub)

type Dimensions =
  { width :: Int
  , height :: Int
  , tileSize :: Int
  }

-- | A DSL for constructing reactions. For internal use only.
-- | You should construct a `Reaction` by calling the different helper functions
-- | in this module, like `modifyW`, instead of building it manually.
data ReactionF world a
  = Lift (Effect a)
  | Modify (world -> world) (world -> a)
  | Dimensions (Dimensions -> a)
  | ExecuteDefaultBehavior (a)

derive instance functorReactionF :: Functor m => Functor (ReactionF world)

type Reaction world = ReactionM world Unit

-- | `ReactionM world a` describes a general reaction in a reactor with the world `world`,
-- | that returns `a`. In the `handleEvent` function we use reactions which don't return anything
-- | (or more precisely, return `Unit`), `ReactionM world Unit`. To simplify things, the following
-- | type synonym is set up.
-- | ```
-- | type Reaction world = ReactionM world Unit
-- | ```
newtype ReactionM world a =
  ReactionM (Free (ReactionF world) a)

derive newtype instance functorReaction :: Functor (ReactionM world)
derive newtype instance applyReaction :: Apply (ReactionM world)
derive newtype instance applicativeReaction :: Applicative (ReactionM world)
derive newtype instance bindReaction :: Bind (ReactionM world)
derive newtype instance monadReaction :: Monad (ReactionM world)
derive newtype instance semigroupReaction :: Semigroup a => Semigroup (ReactionM world a)
derive newtype instance monoidReaction :: Monoid a => Monoid (ReactionM world a)

instance monadEffectReaction :: MonadEffect m => MonadEffect (ReactionM world) where
  liftEffect = ReactionM <<< liftF <<< Lift <<< liftEffect

instance monadRecReaction :: MonadRec (ReactionM world) where
  tailRecM k a =
    k a >>= case _ of
      Loop x -> tailRecM k x
      Done y -> pure y

-- | Get a record of the following:
-- | - `height :: Int`, the height of the grid, in tiles
-- | - `width :: Int`, the width of the grid, in tiles
-- | - `tileSize :: Int` the size of one grid tile, in points. The size is set internally,
-- | and this is the only way to get its value.
dimensions :: forall world. ReactionM world Dimensions
dimensions = ReactionM $ liftF $ Dimensions identity

-- | Modify the current world by passing in a `(world -> world)` updating function.
-- | The function will receive the current value of the world, and should return the new updated value.
-- |
-- | Returns the value of the new world.
-- | ```
-- | type World = { player :: { x :: Int, y :: Int } }
-- |
-- | handleEvent event = case event of
-- |   KeyPress { key : "ArrowUp" } ->
-- |     newWorld <- modifyW \oldWorld ->
-- |       { player: { x : oldWorld.player.x, y: oldWorld.player.y + 1 } }
-- |     log $ "New world is: " <> show newWorld
-- | ```
modifyW :: forall world. (world -> world) -> ReactionM world world
modifyW f = ReactionM $ liftF $ Modify f identity

-- | Similar to `modifyW`, but doesn't return the value of the new world.
modifyW_ :: forall world. (world -> world) -> Reaction world
modifyW_ = map (const unit) <<< modifyW

-- Borrowed from purescript-flame
foreign import unsafeMergeFields
  :: forall world changes
   . Record world
  -> Record changes
  -> Record world

-- | Modify certain fields of the world by passing new values for them.
-- |
-- | Returns the value of the new world.
-- | ```
-- | type World = { player :: { x :: Int, y :: Int } }
-- |
-- | handleEvent event = case event of
-- |   KeyPress { key : "ArrowUp" } ->
-- |     oldWorld <- getW
-- |     newWorld <-
-- |       updateW
-- |         { player: { x : oldWorld.player.x, y: oldWorld.player.y + 1 } }
-- |     log $ "New world is: " <> show newWorld
-- | ```
updateW
  :: forall changes t c world
   . Union changes t world
  => Nub changes c
  => Record changes
  -> ReactionM (Record world) (Record world)
updateW changes = modifyW \world -> (unsafeMergeFields world changes)

-- | Modify certain fields of the world by passing new values for them.
-- | Behaves the same as the `updateW` function, but returns `unit` instead of the new value of the world.
-- |
-- | ```
-- | type World = { player :: { x :: Int, y :: Int } }
-- |
-- | handleEvent event = case event of
-- |   KeyPress { key : "ArrowUp" } -> do
-- |     oldWorld <- getW
-- |     updateW_
-- |       { player: { x : oldWorld.player.x, y: oldWorld.player.y + 1 } }
-- |     newWorld <- getW
-- |     log $ "New world is: " <> show newWorld
-- | ```
updateW_
  :: forall changes t c world
   . Union changes t world
  => Nub changes c
  => Record changes
  -> Reaction (Record world)
updateW_ changes = modifyW_ \world -> (unsafeMergeFields world changes)

-- | Obtain the current value of the world.
-- |
-- | ```
-- | type World = { player :: { x :: Int, y :: Int } }
-- |
-- | handleEvent event = case event of
-- |   KeyPress { key : "Space" } -> do
-- |     w <- getW
-- |     log $ "The player is at position: "
-- |       <> show w.player.x
-- |       <> ", "
-- |       <> show w.player.y
-- | ```
getW :: forall world. ReactionM world world
getW = modifyW identity

-- | After handling the event, execute the default behavior as well.
-- | You can read more in the documentation of `Reactor.Events`.
-- |
-- | Usually you'll call this in the `handleEvent` function, for events that you _don't_ want to handle,
-- | e.g. when pressing 'J' doesn't do anything in your game.
executeDefaultBehavior :: forall world. Reaction world
executeDefaultBehavior = ReactionM $ liftF $ ExecuteDefaultBehavior unit
