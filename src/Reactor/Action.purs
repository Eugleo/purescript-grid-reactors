module Reactor.Action
  ( Action(..)
  , utilities
  , randomPositive
  , randomInRange
  , modify
  , modify_
  , get
  , pause
  , unpause
  , togglePause
  , executeDefaultBehavior
  , ActionF(..)
  , Utilities
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Effect.Class (class MonadEffect, liftEffect)
import Reactor.Events (DefaultBehavior(..))
import Reactor.Graphics.CoordinateSystem (CoordinateSystem, Point)

type Utilities =
  { width :: Int
  , height :: Int
  , tileSize :: Int
  , bound :: CoordinateSystem Point -> CoordinateSystem Point
  }

-- | A DSL for constructing actions. Mostly for internal use.
-- | Most of the time, you construct an action by calling the different helper functions
-- | in this module, like `modify_`, instead of building an `ActionF` manually.
data ActionF m world a
  = RandomNumber Int Int (Int -> a)
  | Lift (m a)
  | Modify (world -> world) (world -> a)
  | Utilities (Utilities -> a)
  | ExecuteDefaultBehavior (a)

derive instance functorActionF :: Functor m => Functor (ActionF m world)

newtype Action m world a =
  Action (Free (ActionF m world) a)

derive newtype instance functorAction :: Functor (Action m world)
derive newtype instance applyAction :: Apply (Action m world)
derive newtype instance applicativeAction :: Applicative (Action m world)
derive newtype instance bindAction :: Bind (Action m world)
derive newtype instance monadAction :: Monad (Action m world)
derive newtype instance semigroupAction :: Semigroup a => Semigroup (Action m world a)
derive newtype instance monoidAction :: Monoid a => Monoid (Action m world a)

instance monadEffectAction :: MonadEffect m => MonadEffect (Action m world) where
  liftEffect = Action <<< liftF <<< Lift <<< liftEffect

instance monadRecAction :: MonadRec (Action m world) where
  tailRecM k a =
    k a >>= case _ of
      Loop x -> tailRecM k x
      Done y -> pure y

-- | Get a record of the following:
-- | - `bound :: CoordinateSystem Point -> CoordinateSystem Point`, a function that bounds
-- | the given point in the grid or the canvas, meaning the point won't be beyond
-- | the bounds of its coordinate system.
-- | - `height :: Int`, `width :: Int`, the dimensions of the grid
-- | - `tileSize :: Int` the size of one grid tile, in points. The size is set internally,
-- | and this is the only way to its value.
utilities :: forall world m. Action m world Utilities
utilities = Action $ liftF $ Utilities identity

-- | Get a random non-negative integer smaller than the given upper bound (inclusive).
-- | The distribution of these integers is discrete uniform.
randomPositive :: forall world m. Int -> Action m world Int
randomPositive = randomInRange 0

-- | Get a random integer between the given bounds (inclusive).
-- | The distribution of these integers is discrete uniform.
randomInRange :: forall world m. Int -> Int -> Action m world Int
randomInRange min max = Action $ liftF $ RandomNumber min max identity

-- | Modify the current world by passing in a `(world -> world)` updating function.
-- | The 'world' is the current state of the reactor.
-- | Returns the value of the new world.
modify :: forall world m. (world -> world) -> Action m world world
modify f = Action $ liftF $ Modify f identity

-- | Modify the current value of the world by passing in a `(world -> world)` updating function.
-- | The 'world' is the current state of the reactor.
-- | Doesn't return anything, as opposed to the `modify` action.
modify_ :: forall world m. (world -> world) -> Action m world Unit
modify_ = map (const unit) <<< modify

-- | Obtain the current value of the world.
get :: forall world m. Action m world world
get = modify identity

-- | Pause the reactor by setting the `paused` attribute in its world to `true`.
-- | A shorthand for
-- | ```
-- | modify_ \w -> w { paused = true }
-- | ```
pause :: forall world m. Action m { paused :: Boolean | world } Unit
pause = modify_ \w -> w { paused = true }

-- | Unpause the reactor by setting the `paused` attribute in its world to `false`.
-- | A shorthand for
-- | ```
-- | modify_ \w -> w { paused = false }
-- | ```
unpause :: forall world m. Action m { paused :: Boolean | world } Unit
unpause = modify_ \w -> w { paused = false }

-- | Toggle the `paused` attribute of the world from `false` to `true` and vice versa.
-- | A shorthand for
-- | ```
-- | modify_ \w -> w { paused = not w.paused }
-- | ```
togglePause :: forall world m. Action m { paused :: Boolean | world } Unit
togglePause = modify_ \w -> w { paused = not w.paused }

-- | After handling the event, execute the default behavior as well.
-- | You can read more in the documentation of `Reactor.Events`.
-- |
-- | Usually you'll call this in the `handleEvent` function, for events that you _don't_ want to handle,
-- | e.g. when pressing 'J' doesn't 'do anything' in your game.
executeDefaultBehavior
  :: forall world m. Action m { paused :: Boolean | world } Unit
executeDefaultBehavior = Action $ liftF $ ExecuteDefaultBehavior unit
