module Reactor.Events
  ( optionallyPreventDefault
  , DefaultBehavior(..)
  , mouseEventFromDOM
  , MouseEvent(..)
  , MouseEventType(..)
  , keypressEventFromDOM
  , KeypressEvent(..)
  , TickEvent(..)
  , windowPerformanceNow
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (HookM)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (Window)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

data DefaultBehavior = Prevent | Execute
derive instance eqDefaultBehavior :: Eq DefaultBehavior

optionallyPreventDefault
  :: forall m. MonadEffect m => DefaultBehavior -> Event -> HookM m Unit
optionallyPreventDefault behavior =
  when (behavior == Prevent)
    <<< liftEffect
    <<< preventDefault

newtype TickEvent =
  TickEvent { delta :: Number }
foreign import windowPerformanceNow :: Window -> Effect Number

data MouseEventType
  = ButtonUp
  | ButtonDown
  | Drag
  | Enter
  | Leave
  | Move

derive instance eqMouseEventType :: Eq MouseEventType
derive instance genericMouseEventType :: Generic MouseEventType _
instance showMyADT :: Show MouseEventType where
  show = genericShow

data MouseEvent = MouseEvent
  { type :: MouseEventType
  , x :: Int
  , y :: Int
  , control :: Boolean
  , meta :: Boolean
  , alt :: Boolean
  , shift :: Boolean
  , button :: Int
  }

derive instance genericMouseEvent :: Generic MouseEvent _
instance showMouseEvent :: Show MouseEvent where
  show = genericShow

foreign import offsetX :: ME.MouseEvent -> Int
foreign import offsetY :: ME.MouseEvent -> Int

mouseEventFromDOM
  :: { cellSize :: Int, width :: Int, height :: Int }
  -> MouseEventType
  -> ME.MouseEvent
  -> MouseEvent
mouseEventFromDOM { cellSize, width, height } eventType event =
  MouseEvent
    { type: eventType
    , x: clip (offsetX event / cellSize) (height - 1)
    , y: clip (offsetY event / cellSize) (width - 1)
    , control: ME.ctrlKey event
    , alt: ME.altKey event
    , meta: ME.metaKey event
    , shift: ME.shiftKey event
    , button: ME.button event
    }
  where
  clip n b = max (min n b) 0

data KeypressEvent
  = KeypressEvent String
  { shift :: Boolean
  , control :: Boolean
  , alt :: Boolean
  , meta :: Boolean
  }

keypressEventFromDOM :: KE.KeyboardEvent -> KeypressEvent
keypressEventFromDOM event =
  KeypressEvent (KE.key event)
    { shift: KE.shiftKey event
    , control: KE.ctrlKey event
    , alt: KE.altKey event
    , meta: KE.metaKey event
    }
