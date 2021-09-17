-- | This module defines the three types of events that the reactor needs to react to.
module Reactor.Events
  ( optionallyPreventDefault
  , DefaultBehavior(..)
  , mouseEventFromDOM
  , MouseEventType(..)
  , keypressEventFromDOM
  , Event(..)
  , windowPerformanceNow
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (HookM)
import Reactor.Graphics.CoordinateSystem (CoordinateSystem, grid, relativeTo)
import Web.Event.Event (Event, preventDefault) as Web
import Web.HTML (Window)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-- | This datatype is used internally to signal to the reactor whether you want to prevent the
-- | default behavior that would normally happen as a response to the event.
-- |
-- | In more detail: Some events have default behaviors associated with them; for example, pressing Ctrl+F
-- | shows the find on the page dialog for the current page. When handling events, we often want to
-- | 'consume' them, and _prevent_ the default behavior from happening — maybe Ctrl+F is used
-- | for the 'fire' command in your game, and you don't want the find on the page dialog to show.
-- |
-- | The default behavior is prevented automatically in your event handlers (`onKey`, `onMouse`).
-- | If you want to execture the default behavior for certain events, you have to explicitly call
-- | Reactor.Action.executeDefaultBehavior.
data DefaultBehavior = Prevent | Execute

derive instance eqDefaultBehavior :: Eq DefaultBehavior

-- | Used internally.
optionallyPreventDefault
  :: forall m. MonadEffect m => DefaultBehavior -> Web.Event -> HookM m Unit
optionallyPreventDefault behavior =
  when (behavior == Prevent)
    <<< liftEffect
    <<< Web.preventDefault

-- | The main mouse event types, from the point of view of the rendered grid.
-- | A `Drag` occurs when the mouse button is down during a `Move`.
-- | Right-button clicks are not handled, those get handled by the browser.
data MouseEventType
  = ButtonUp
  | ButtonDown
  | Drag
  | Enter
  | Leave
  | Move

-- | This types describes the different events that occur during the life of a reactor. You
-- | pattern match on this in the `handleEvent` function.
-- |
-- | ## Tick Event
-- | This event is fired on every clock-tick in the reactor.
-- | The attribute `delta` is the time from the last tick, in seconds.
-- | There's approximately 60 ticks per second, however, the number _can_ vary depending on the browser's current available resources.
-- | For the smoothest motion, you should calculate the traveled distance based on the
-- | speed of the entity and the `delta`.
-- |
-- | ## Mouse Event
-- | This event is fired whenever the user presses a button on their mouse, or moves the mouse over the rendering grid.
-- | Explanation of each of the fields:
-- | - `type` is the type of the event (drag, button up, etc.)
-- | - `x` and `y` are the coordinates of the event. They are relative to the rendering grid,
-- | and denote the _tile_ where the event hapened.
-- | - `control`, `meta`, `shift`, and `alt` denote whether any modifier keys were pressed when the event happened
-- | - `button` is an identifier of the button that has been pressed during the event, if applicable.
-- | See this [entry in MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button) for more details.
-- |
-- | ## Keypress Event
-- | This event is fired whenever the user presses down a key on the keyboard.
-- | The key is passed in the event as a `String`, with the following rules:
-- | - Numbers, letters, and symbols all have intuitive codes (type A → get "A", type ů → get "ů").
-- | Notably, pressing space produces an event with a literal space, `" "`.
-- | - Any modifier keys that were pressed simultaneously with the main key are passed in the record
-- | `{ shift, control, alt, meta }`. A `meta` key is the Windows button on Windows, and the Command button on the Mac.
-- | - Common special keys have intuitive names. For example: `ArrowLeft`, `ArrowRight`, `ArrowUp`,
-- | `ArrowDown`. Full list can be seen on [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values).

data Event
  = TickEvent { delta :: Number }
  | MouseEvent
      { type :: MouseEventType
      , gridCoords :: CoordinateSystem { x :: Number, y :: Number }
      , control :: Boolean
      , meta :: Boolean
      , alt :: Boolean
      , shift :: Boolean
      , button :: Int
      }
  | KeypressEvent String
      { shift :: Boolean
      , control :: Boolean
      , alt :: Boolean
      , meta :: Boolean
      }

foreign import windowPerformanceNow :: Window -> Effect Number

derive instance genericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow

derive instance eqMouseEventType :: Eq MouseEventType
derive instance genericMouseEventType :: Generic MouseEventType _
instance showMyADT :: Show MouseEventType where
  show = genericShow

foreign import offsetX :: ME.MouseEvent -> Int
foreign import offsetY :: ME.MouseEvent -> Int

-- | Convert a DOM mouse event into our custom event type. Used internally, you shouldn't need it.
mouseEventFromDOM
  :: { tileSize :: Int, width :: Int, height :: Int }
  -> MouseEventType
  -> ME.MouseEvent
  -> Event
mouseEventFromDOM { tileSize, width, height } eventType event =
  MouseEvent
    { type: eventType
    , gridCoords:
        { x: clip (offsetX event / tileSize) (height - 1)
        , y: clip (offsetY event / tileSize) (width - 1)
        } `relativeTo` grid
    , control: ME.ctrlKey event
    , alt: ME.altKey event
    , meta: ME.metaKey event
    , shift: ME.shiftKey event
    , button: ME.button event
    }
  where
  clip n b = max (min n b) 0

-- | Convert a DOM keyboard event into our custom event type. Used internally, you shouldn't need it.
keypressEventFromDOM :: KE.KeyboardEvent -> Event
keypressEventFromDOM event =
  KeypressEvent (KE.key event)
    { shift: KE.shiftKey event
    , control: KE.ctrlKey event
    , alt: KE.altKey event
    , meta: KE.metaKey event
    }
