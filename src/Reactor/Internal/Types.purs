module Reactor.Internal.Types (Cell(..), State, Properties) where

import Prelude

import Data.Grid (Grid)
import Data.Maybe (Maybe)
import Graphics.CanvasAction (Context2D)
import Halogen.Hooks (HookM)
import Halogen.Subscription (Listener)
import Reactor.Action (Action)
import Reactor.Events (KeypressEvent, MouseEvent, TickEvent, DefaultBehavior)
import Reactor.Graphics.Drawing (Drawing)
import Color (Color)

data Cell = Colored Color | EmptyCell

derive instance eqCell :: Eq Cell

type Properties m world =
  { title :: String
  , width :: Int
  , height :: Int
  , tileSize :: Int
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