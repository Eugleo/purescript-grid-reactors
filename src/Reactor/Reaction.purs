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

-- | A DSL for constructing reactions. Mostly for internal use.
-- | Most of the time, you construct an action by calling the different helper functions
-- | in this module, like `modify_`, instead of building an `ReactionF` manually.
data ReactionF world a
  = Lift (Effect a)
  | Modify (world -> world) (world -> a)
  | Dimensions (Dimensions -> a)
  | ExecuteDefaultBehavior (a)

derive instance functorReactionF :: Functor m => Functor (ReactionF world)

type Reaction world = ReactionM world Unit

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
-- | - `height :: Int`, `width :: Int`, the dimensions of the grid
-- | - `tileSize :: Int` the size of one grid tile, in points. The size is set internally,
-- | and this is the only way to get its value.
dimensions :: forall world. ReactionM world Dimensions
dimensions = ReactionM $ liftF $ Dimensions identity

-- | Modify the current world by passing in a `(world -> world)` updating function.
-- | The 'world' is the current state of the reactor.
-- | Returns the value of the new world.
modifyW :: forall world. (world -> world) -> ReactionM world world
modifyW f = ReactionM $ liftF $ Modify f identity

-- | Modify the current value of the world by passing in a `(world -> world)` updating function.
-- | The 'world' is the current state of the reactor.
-- | Doesn't return anything, as opposed to the `modify` action.
modifyW_ :: forall world. (world -> world) -> Reaction world
modifyW_ = map (const unit) <<< modifyW

-- Borrowed from purescript-flame
foreign import unsafeMergeFields
  :: forall world changes
   . Record world
  -> Record changes
  -> Record world

updateW
  :: forall changes t c world
   . Union changes t world
  => Nub changes c
  => Record changes
  -> ReactionM (Record world) (Record world)
updateW changes = modifyW \world -> (unsafeMergeFields world changes)

updateW_
  :: forall changes t c world
   . Union changes t world
  => Nub changes c
  => Record changes
  -> Reaction (Record world)
updateW_ changes = modifyW_ \world -> (unsafeMergeFields world changes)

-- | Obtain the current value of the world.
getW :: forall world. ReactionM world world
getW = modifyW identity

-- | After handling the event, execute the default behavior as well.
-- | You can read more in the documentation of `Reactor.Events`.
-- |
-- | Usually you'll call this in the `handleEvent` function, for events that you _don't_ want to handle,
-- | e.g. when pressing 'J' doesn't do anything in your game.
executeDefaultBehavior :: forall world. Reaction world
executeDefaultBehavior = ReactionM $ liftF $ ExecuteDefaultBehavior unit
