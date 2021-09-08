-- | This module introduces the `Reactor` as a datatype.
-- | As the [Pyret documentation](https://www.pyret.org/docs/latest/reactors.html) puts it,
-- | a reactor is a value enabling the creation of time-based animations, simulations, and interactive programs.
-- | During the creation of a reactor, the user supplies a function for handling clock ticks,
-- | and functions for handling mouse and keyboard input events.
-- | The reactor calls these event handlers whenever the respective event occurs.
-- | From within the event handlers, the reactor's state — or, the _world_, as we call it —
-- | can be updated. After each world update, the reactor renders the new world with the supplied drawing function.

module Reactor.Types (Reactor, Configuration) where

import Data.Unit (Unit)
import Reactor.Action (Action)
import Reactor.Events (KeypressEvent, MouseEvent, TickEvent, DefaultBehavior)
import Reactor.Graphics.Drawing (Drawing)

-- | The reactor is a simple record. A reactor is is parametrized over the following type variables:
-- | - `m` signifies which monad will be used to run
-- | the event handlers, which is mostly an implementation detail.
-- | Usually `m` is `Effect` or `Aff` (i.e. asynchronous `Effect`). You can safely ignore this most of the time.
-- | - `world` denotes what is the type of the internal state of the reactor.
-- |This is where the state of your game or simulation is saved.
-- |
-- | The fields in the record are the following:
-- | - `init` is the initial state of the reactor's world
-- | - `draw` is a function used to render the world anytime it is changed
-- | - `onTick` is a function called on every tick of the reactor's clock.
-- | It is called around 60 times a second, provided the reactor is not paused.
-- | - `onKey` is an event handler for keyboard input events. It receives the pressed key
-- | and does some `Action`, which usually involves updating the world based on the received
-- | input.
-- | `onMouse` is an event handler for mouse input events (button clicks, draggging, moving).
-- | Similarly to the `onKey` function, it receives the details of the event
-- | and does some `Action`, which usually involves updating the world based on the event.
-- |
-- | For example, when ran, the following reactor would render a player at the initial position,
-- | and would allow the user to move the player by pressing arrow keys. The clock is paused,
-- | and mouse events are ignored.
-- | ```haskell
-- | import Reactor.Action
-- |   (executeDefaultBehavior, modify_, preventDefaultBehavior, utilities)
-- | import Reactor.Events (KeypressEvent(..))
-- | import Reactor.Graphics.Colors as Color
-- | import Reactor.Graphics.CoordinateSystem
-- |   (CoordinateSystem, grid, moveDown, moveLeft, moveRight, moveUp, wrt)
-- | import Reactor.Graphics.Drawing (fill, cell)
-- | import Reactor.Types (Reactor)
-- |
-- | type World =
-- |   { player :: CoordinateSystem { x :: Number, y :: Number }
-- |   , paused :: Boolean
-- |   }
-- |
-- | reactor :: forall m. Reactor m World
-- | reactor =
-- |   { init: { player: { x: 0, y: 0 } `wrt` grid, paused: true }
-- |   , draw: \{ player } -> fill Color.blue400 $ cell player
-- |   , onTick: \_ -> pure unit
-- |   , onKey: \(KeypressEvent key _) -> do
-- |       { bound } <- utilities
-- |       case key of
-- |         "ArrowLeft" -> do
-- |           modify_ \w@{ player } ->
-- |             w { player = bound $ moveLeft player }
-- |           preventDefaultBehavior
-- |         "ArrowRight" -> do
-- |           modify_ \w@{ player } ->
-- |             w { player = bound $ moveRight player }
-- |           preventDefaultBehavior
-- |         "ArrowDown" -> do
-- |           modify_ \w@{ player } ->
-- |             w { player = bound $ moveDown player }
-- |           preventDefaultBehavior
-- |         "ArrowUp" -> do
-- |           modify_ \w@{ player } ->
-- |             w { player = bound $ moveUp player }
-- |           preventDefaultBehavior
-- |         _ -> executeDefaultBehavior
-- |   , onMouse: \_ -> executeDefaultBehavior
-- |   }
-- | ```
type Reactor m world =
  { init :: world
  , draw :: world -> Drawing
  , onTick :: TickEvent -> Action m world Unit
  , onKey :: KeypressEvent -> Action m world DefaultBehavior
  , onMouse :: MouseEvent -> Action m world DefaultBehavior
  }

-- | Configuration for the Halogen component that renders the reactor. Although a reactor
-- | is a general structure, our reactors focus on grid-based games and simulations.
-- | The component thus needs a little more info than just what is in already the reactor, namely:
-- | - `title`, the title of the webpage where the reactor is rendered
-- | - `width`, the width of the grid, in cells
-- | - `height` the height of the grid, in cells
-- | Notably, the size of the rendered cells isn't customizable and is set to 30pts.
type Configuration =
  { title :: String
  , width :: Int
  , height :: Int
  }