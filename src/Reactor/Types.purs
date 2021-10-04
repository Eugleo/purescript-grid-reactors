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
import Reactor.Reaction (Reaction)
import Reactor.Events (Event)
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
-- | - `handleEvent` is a function for handling the three types of events: keypress events, mouse events, and tick events.
-- | The events are handled by running the action returned by `handleEvent`.
-- |   - Tick events are fired on every tick of the reactor's clock, around 60 times a second, provided the reactor is not paused.
-- |   - Keypress events are fired when a key on a keyboard is pressed.
-- |   - Mouse events are fired whenever a mouse is moved above the canvas (the drawing area of the reactor), when the canvas is clicked, or when the user drags something (i.e. clicks and then moves the mouse).
-- |
-- | For example, when ran, the following reactor would render a player at the initial position,
-- | and would allow the user to move the player by pressing arrow keys. The clock is paused,
-- | and mouse events are ignored.
-- | ```haskell
-- | import Reactor.Action
-- |   (executeDefaultBehavior, modify_, utilities)
-- | import Reactor.Events (KeypressEvent(..))
-- | import Reactor.Graphics.Colors as Color
-- | import Reactor.Graphics.CoordinateSystem
-- |   (CoordinateSystem, grid, moveDown, moveLeft, moveRight, moveUp, relativeTo)
-- | import Reactor.Graphics.Drawing (fill, tile)
-- | import Reactor.Types (Reactor)
-- |
-- | type World =
-- |   { player :: CoordinateSystem { x :: Number, y :: Number }
-- |   , paused :: Boolean
-- |   }
-- |
-- | reactor :: forall m. Reactor m World
-- | reactor =
-- |   { init: { player: { x: 0, y: 0 } `relativeTo` grid, paused: true }
-- |   , draw: \{ player } -> fill Color.blue400 $ tile player
-- |   , handleEvent: \event -> do
-- |       { bound } <- utilities
-- |       case event of
-- |         KeypressEvent "ArrowLeft" _ ->
-- |           modify_ \w -> w { player = bound $ moveLeft w.player }
-- |         KeypressEvent "ArrowRight" _ ->
-- |           modify_ \w -> w { player = bound $ moveRight w.player }
-- |         KeypressEvent "ArrowDown" _ ->
-- |           modify_ \w -> w { player = bound $ moveDown w.player }
-- |         KeypressEvent "ArrowUp" _ ->
-- |           modify_ \w -> w { player = bound $ moveUp w.player }
-- |         KeypressEvent " " _ -> togglePause
-- |         _ -> executeDefaultBehavior
-- |   }
-- | ```
type Reactor m world =
  { init :: world
  , draw :: world -> Drawing
  , isPaused :: world -> Boolean
  , handleEvent :: Event -> Reaction m world Unit
  }

-- | Configuration for the Halogen component that renders the reactor. Although a reactor
-- | is a general structure, our reactors focus on grid-based games and simulations.
-- | The component thus needs a little more info than just what is in already the reactor, namely:
-- | - `title`, the title of the webpage where the reactor is rendered
-- | - `width`, the width of the grid, in tiles
-- | - `height` the height of the grid, in tiles
-- | Notably, the size of the rendered tiles isn't customizable and is set to 30pts.
type Configuration =
  { title :: String
  , width :: Int
  , height :: Int
  }