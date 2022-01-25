module Example.Gol.Main
  ( Cell(..)
  , World
  , draw
  , handleEvent
  , height
  , initial
  , main
  , reactor
  , width
  ) where

import Prelude

import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactor (Reactor, executeDefaultBehavior, getW, modifyW, modifyW_, runReactor, updateW_)
import Reactor.Events (Event(..), MouseInteractionType(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Internal.Helpers (withJust)
import Reactor.Internal.Widget (Widget(..))
import Reactor.Reaction (Reaction, widget)

width :: Int
width = 20

height :: Int
height = 20

main :: Effect Unit
main = runReactor reactor
  { title: "Game of Life"
  , width
  , height
  , widgets:
      [ ("section-1" /\ Section { title: "Update frequency" })
      , ("frequency" /\ Label { content: show 40 })
      , ("section-2" /\ Section { title: "Number of generations" })
      , ("generation" /\ Label { content: show 0 })
      ]
  }

data Cell = Dead | Alive Int

derive instance eqCell :: Eq Cell

type World =
  { cells :: Grid Cell
  , cursor :: Maybe Coordinates
  , paused :: Boolean
  , time :: Int
  , generation :: Int
  , speed :: Int
  , lastEdited :: Maybe Coordinates
  }

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: \world -> world.paused }

initial :: World
initial =
  { cells: Grid.replicate width height Dead
  , cursor: Nothing
  , paused: true
  , time: 0
  , generation: 0
  , speed: 40
  , lastEdited: Nothing
  }

draw :: World -> Drawing
draw { cells, cursor } = do
  withJust cursor \position -> fill Color.gray200 $ tile position
  drawGrid cells $ \cell ->
    case cell of
      Dead -> Nothing
      Alive born -> Just $ Color.hsl (toNumber born) 0.6 0.75

handleEvent :: Event -> Reaction World
handleEvent event = case event of
  KeyPress { key: " " } -> togglePause
  KeyPress { key: "ArrowDown" } -> updateSpeed (-1)
  KeyPress { key: "ArrowUp" } -> updateSpeed 1

  Mouse { type: Move, position } ->
    updateW_ { cursor: Just $ position }
  Mouse { type: ButtonDown, position } -> toggleCell position
  Mouse { type: Drag, position } -> toggleCell position

  Tick _ -> advanceWorld

  _ -> executeDefaultBehavior

  where
  updateSpeed d = do
    world <- getW
    widget "frequency" $ Label { content: show world.frequency }

  togglePause = modifyW_ \world -> world { paused = not world.paused }
  toggleCell position = do
    { cells: cs, time, lastEdited } <- getW
    when (Just position /= lastEdited) $
      updateW_
        { cells: Grid.modifyAt' position (toggle time) cs
        , lastEdited: Just position
        }
  toggle time cell = case cell of
    Dead -> Alive time
    Alive _ -> Dead

advanceWorld :: Reaction World
advanceWorld = do
  { cells, time, speed, generation } <- getW
  modifyW_ \w -> w { time = w.time + 1 }
  when ((toNumber time / 60.0) > 1.0 / toNumber speed) $ do
    updateW_
      { cells: mapWithIndex (modifyCell cells time) cells
      , generation: generation + 1
      , time: 0
      }
    widget "generation" $ Label { content: show generation }
  where
  modifyCell cells time position cell =
    let
      ns = neigborCount cells position
    in
      case cell of
        Dead -> if ns == 3 then Alive time else Dead
        Alive born -> if ns == 2 || ns == 3 then Alive born else Dead

  neigborCount cells { x, y } =
    Array.length
      $ Array.filter (\c -> isJust c && c /= Just Dead)
      $ map (Grid.index cells) do
          a <- [ -1, 0, 1 ]
          b <- [ -1, 0, 1 ]
          if (a /= 0 || b /= 0) then
            pure { x: x + a, y: y + b }
          else []
