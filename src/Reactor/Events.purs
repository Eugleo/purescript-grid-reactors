-- | This module defines the three types of events that the reactor needs to react to.
module Reactor.Events
  ( optionallyPreventDefault
  , DefaultBehavior(..)
  , mouseEventFromDOM
  , MouseInteractionType(..)
  , keypressEventFromDOM
  , Event(..)
  , Modifiers
  , windowPerformanceNow
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Grid (Coordinates)
import Data.Int (toNumber)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (HookM)
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
-- | The default behavior is prevented automatically in your event handler.
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
data MouseInteractionType
  = ButtonUp
  | ButtonDown
  | Drag
  | Enter
  | Leave
  | Move

-- | This type describes the different events that occur during the life of a reactor. You
-- | pattern match on this in the `handleEvent` function.
-- |
-- | ## Tick Event
-- | This event is fired on every clock-tick in the reactor.
-- | The attribute `delta` is the time from the last tick, in seconds.
-- | There's approximately 60 ticks per second, however, the number _can_ vary depending on the browser's current available resources.
-- |
-- | ## Mouse Event
-- | This event is fired whenever the user presses a button on their mouse, or moves the mouse over the rendering grid.
-- | Explanation of each of the fields:
-- | - `type` is the type of the event (drag, button up, etc.)
-- | - `position` is the position of the event relative to the rendering grid (i.e. in tiles).
-- | - `positionOnCanvas` is the position of the event relative to the rendering canvas (i.e. in pixels).
-- | - for more information on `modifiers` see the documentation of the type `Modifiers`.
-- |
-- | ## Keypress Event
-- | This event is fired whenever the user presses down a key on the keyboard.
-- | The key is passed in the event as a `String`, with the following rules:
-- | - Numbers, letters, and symbols all have intuitive codes (type A → get "A", type ů → get "ů").
-- | Notably, pressing space produces an event with a literal space, `" "`.
-- | - Any modifier keys that were pressed simultaneously with the main key are passed in the record `modifiers`.
-- | - Common special keys have intuitive names. For example: `ArrowLeft`, `ArrowRight`, `ArrowUp`,
-- | `ArrowDown`. Full list can be seen on [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values).

data Event
  = Tick { delta :: Number }
  | Mouse
      { type :: MouseInteractionType
      , position :: Coordinates
      , positionOnCanvas :: { x :: Number, y :: Number }
      , modifiers :: Modifiers
      }
  | KeyPress { key :: String, modifiers :: Modifiers }

-- | `Mouse` and `KeyPress` events carry the info about modifier keys that were pressed
-- | when the event happened.
-- |  A `meta` key is the Windows button on Windows, and the Command button on the Mac.
-- | The rest are the common modifier keys.
type Modifiers =
  { control :: Boolean
  , meta :: Boolean
  , alt :: Boolean
  , shift :: Boolean
  }

foreign import windowPerformanceNow :: Window -> Effect Number

derive instance genericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow

derive instance eqMouseInteractionType :: Eq MouseInteractionType
derive instance genericMouseInteractionType :: Generic MouseInteractionType _
instance showMouseInteractionType :: Show MouseInteractionType where
  show = genericShow

foreign import offsetX :: ME.MouseEvent -> Int
foreign import offsetY :: ME.MouseEvent -> Int

-- | Convert a DOM mouse event into our custom event type. Used internally, you shouldn't need it.
mouseEventFromDOM
  :: { width :: Int, height :: Int, tileSize :: Int }
  -> MouseInteractionType
  -> ME.MouseEvent
  -> Event
mouseEventFromDOM { width, height, tileSize } eventType event =
  Mouse
    { type: eventType
    , position:
        { x: clip (offsetX event / tileSize) (height - 1)
        , y: clip (offsetY event / tileSize) (width - 1)
        }
    , positionOnCanvas:
        { x: toNumber $ offsetX event, y: toNumber $ offsetY event }
    , modifiers:
        { control: ME.ctrlKey event
        , alt: ME.altKey event
        , meta: ME.metaKey event
        , shift: ME.shiftKey event
        }
    }
  where
  clip n b = max (min n b) 0

-- | Convert a DOM keyboard event into our custom event type. Used internally, you shouldn't need it.
keypressEventFromDOM :: KE.KeyboardEvent -> Event
keypressEventFromDOM event =
  KeyPress
    { key: KE.key event
    , modifiers:
        { shift: KE.shiftKey event
        , control: KE.ctrlKey event
        , alt: KE.altKey event
        , meta: KE.metaKey event
        }
    }
