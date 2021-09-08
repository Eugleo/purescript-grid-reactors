module Reactor.Types where

import Data.Grid (Grid)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Graphics.CanvasAction (Context2D)
import Halogen.Hooks (HookM)
import Halogen.Subscription (Listener)
import Reactor.Action (Action)
import Reactor.Events (KeypressEvent, MouseEvent, TickEvent, DefaultBehavior)
import Reactor.Graphics.Drawing (Drawing)
import Reactor.Internal.Types (Cell)

type Reactor m world =
  { title :: String
  , init :: world
  , draw :: world -> Drawing
  , onTick :: TickEvent -> Action m world Unit
  , onKey :: KeypressEvent -> Action m world DefaultBehavior
  , onMouse :: MouseEvent -> Action m world DefaultBehavior
  , width :: Int
  , height :: Int
  }

type Properties m world =
  { title :: String
  , width :: Int
  , height :: Int
  , cellSize :: Int
  , draw :: world -> Drawing
  , onTick :: TickEvent -> Action m world Unit
  , onKey :: KeypressEvent -> Action m world DefaultBehavior
  , onMouse :: MouseEvent -> Action m world DefaultBehavior
  }

type State m world =
  { context :: Maybe Context2D
  , mouseButtonPressed :: Boolean
  , renderListener :: Maybe (Listener (HookM m Unit))
  , lastTick :: Number
  , lastGrid :: Maybe (Grid Cell)
  , world :: world
  }