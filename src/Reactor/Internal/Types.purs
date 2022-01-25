module Reactor.Internal.Types (Cell(..), State, Properties) where

import Prelude

import Color (Color)
import Data.Grid (Grid)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Graphics.CanvasAction (Context2D)
import Halogen.Hooks (HookM)
import Halogen.Subscription (Listener)
import Reactor.Events (Event)
import Reactor.Graphics.Drawing (Drawing)
import Reactor.Internal.Widget (Widget)
import Reactor.Reaction (Reaction)

data Cell = Colored Color | EmptyCell

derive instance eqCell :: Eq Cell

type Properties world =
  { title :: String
  , width :: Int
  , height :: Int
  , tileSize :: Int
  , draw :: world -> Drawing
  , isPaused :: world -> Boolean
  , handleEvent :: Event -> Reaction world
  }

type State m world =
  { context :: Maybe Context2D
  , mouseButtonPressed :: Boolean
  , renderListener :: Maybe (Listener (HookM m Unit))
  , lastTick :: Number
  , lastGrid :: Maybe (Grid Cell)
  , world :: world
  , widgets :: Array (String /\ Widget)
  }