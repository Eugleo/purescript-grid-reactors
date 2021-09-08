module Reactor.Action where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Effect.Class (class MonadEffect, liftEffect)
import Reactor.Events (DefaultBehavior(..))
import Reactor.Graphics.CoordinateSystem (CoordinateSystem)
import Reactor.Graphics.Drawing (Point)

type Utilities =
  { width :: Int
  , height :: Int
  , cellSize :: Int
  , bound :: CoordinateSystem Point -> Point
  }

data ActionF m world a
  = RandomNumber Int Int (Int -> a)
  | Lift (m a)
  | Modify (world -> world) (world -> a)
  | Utilities (Utilities -> a)

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

utilities :: forall world m. Action m world Utilities

utilities = Action $ liftF $ Utilities identity

randomPositive :: forall world m. Int -> Action m world Int
randomPositive = randomInRange 0

randomInRange :: forall world m. Int -> Int -> Action m world Int
randomInRange min max = Action $ liftF $ RandomNumber min max identity

modify :: forall world m. (world -> world) -> Action m world world
modify f = Action $ liftF $ Modify f identity

modify_ :: forall world m. (world -> world) -> Action m world Unit
modify_ = map (const unit) <<< modify

get :: forall world m. Action m world world
get = modify identity

pause :: forall world m. Action m { paused :: Boolean | world } Unit
pause = Action $ liftF $ Modify (\s -> s { paused = true }) (const unit)

unpause :: forall world m. Action m { paused :: Boolean | world } Unit
unpause = Action $ liftF $ Modify (\s -> s { paused = false }) (const unit)

triggerPause :: forall world m. Action m { paused :: Boolean | world } Unit
triggerPause = Action $ liftF $ Modify (\s -> s { paused = not s.paused }) (const unit)

preventDefaultBehavior
  :: forall world m. Action m { paused :: Boolean | world } DefaultBehavior
preventDefaultBehavior = Action $ liftF $ Modify identity (const Prevent)

executeDefaultBehavior
  :: forall world m. Action m { paused :: Boolean | world } DefaultBehavior
executeDefaultBehavior = Action $ liftF $ Modify identity (const Execute)
