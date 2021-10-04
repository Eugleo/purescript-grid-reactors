module Reactor.Internal.Types (Cell(..), State, Properties) where

import Prelude

import Data.Grid (Grid)
import Data.Maybe (Maybe)
import Graphics.CanvasAction (Context2D)
import Halogen.Hooks (HookM)
import Halogen.Subscription (Listener)
import Reactor.Reaction (Reaction)
import Reactor.Events (Event)
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
  , isPaused :: world -> Boolean
  , handleEvent :: Event -> Reaction m world Unit
  }

type State m world =
  { context :: Maybe Context2D
  , mouseButtonPressed :: Boolean
  , renderListener :: Maybe (Listener (HookM m Unit))
  , lastTick :: Number
  , lastGrid :: Maybe (Grid Cell)
  , world :: world
  }