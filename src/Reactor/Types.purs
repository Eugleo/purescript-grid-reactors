-- | This module introduces the `Reactor` as a datatype.
-- | As the [Pyret documentation](https://www.pyret.org/docs/latest/reactors.html) puts it,
-- | a reactor is a value enabling the creation of time-based animations, simulations, and interactive programs.
-- | During the creation of a reactor, the user supplies a function for handling clock ticks,
-- | and functions for handling mouse and keyboard input events.
-- | The reactor calls these event handlers whenever the respective event occurs.
-- | From within the event handlers, the reactor's state — or, the _world_, as we call it —
-- | can be updated. After each world update, the reactor renders the new world with the supplied drawing function.

module Reactor.Types (Reactor, Configuration) where

import Reactor.Reaction (Reaction)
import Reactor.Events (Event)
import Reactor.Graphics.Drawing (Drawing)

-- | The reactor is a simple record. Reactors are parametrized over the type of the `world` which is saved in the reactor.
-- |
-- | The fields in the record are the following:
-- | - `initial` is the initial state of the reactor's world
-- | - `isPaused` is a function used by the reactor to find out whether its internal clock should be paused or not. The clock is paused when `isPaused` returns `true`.
-- | - `draw` is a function used to render the world anytime it is changed
-- | - `handleEvent` is a function for handling the three types of events that can occur during your game or simulation:
-- | keypress events, mouse events, and tick events.
-- | The events are handled by running the `Reaction` returned by `handleEvent`.
-- |   - Tick events are fired on every tick of the reactor's clock, around 60 times a second, provided the reactor is not paused.
-- |   - Keypress events are fired when a key on a keyboard is pressed.
-- |   - Mouse events are fired whenever a mouse is moved above the canvas (the drawing area of the reactor),
-- | when the canvas is clicked, or when the user drags something (i.e. clicks and then moves the mouse).
-- | ```
type Reactor world =
  { initial :: world
  , draw :: world -> Drawing
  , isPaused :: world -> Boolean
  , handleEvent :: Event -> Reaction world
  }

-- | Configuration for the Halogen component that renders the reactor.
-- | The component creates everything on the webpage you see when you run the reactor.
-- | Although a reactor
-- | is a general structure, this library focuses on reactors with grid-based rendering.
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