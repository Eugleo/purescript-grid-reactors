module Example.Gol.Main where

import Prelude

import Data.Array as Array
import Data.Grid (Grid)
import Data.Grid as Grid
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Reactor (Event(..), Reactor, executeDefaultBehavior, fill, runReactor, tile, updateW_)
import Reactor.Events (MouseInteractionType(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, Point, drawGrid)
import Reactor.Internal.Helpers (withJust)
import Reactor.Reaction (Reaction, getW, modifyW_)

width :: Int
width = 20

height :: Int
height = 20

main :: Effect Unit
main = runReactor reactor { title: "Game of Life", width, height }

data Cell = Dead | Alive Int

derive instance eqCell :: Eq Cell

type World = { cells :: Grid Cell, cursor :: Maybe Point, paused :: Boolean, time :: Int, speed :: Int }

reactor :: Reactor World
reactor = { init, draw, handleEvent, isPaused: \world -> world.paused }

init :: World
init = { cells: Grid.replicate width height Dead, cursor: Nothing, paused: true, time: 0, speed: 60 }

draw :: World -> Drawing
draw { cells, cursor } = do
  drawGrid cells $ \cell ->
    case cell of
      Dead -> Nothing
      Alive born -> Just $ Color.hsl (toNumber born) 0.6 0.75
  withJust cursor \position -> fill Color.gray200 $ tile position

handleEvent :: Event -> Reaction World
handleEvent event = case event of
  KeyPress { key: " " } -> togglePause
  KeyPress { key: "ArrowDown" } -> modifyW_ \w -> w { speed = max (w.speed - 10) 0 }
  KeyPress { key: "ArrowUp" } -> modifyW_ \w -> w { speed = w.speed + 10 }

  Mouse { type: Move, position } ->
    updateW_ { cursor: Just $ position }
  Mouse { type: ButtonDown, position } -> toggleCell position
  Mouse { type: Drag, position } -> toggleCell position

  Tick _ -> advanceWorld

  _ -> executeDefaultBehavior

  where
  togglePause = modifyW_ \world -> world { paused = not world.paused }
  toggleCell position = do
    { cells: cs, time } <- getW
    updateW_ { cells: Grid.modifyAt' position (toggle time) cs }
  toggle time cell = case cell of
    Dead -> Alive time
    Alive _ -> Dead

advanceWorld :: Reaction World
advanceWorld = do
  { cells, time, speed } <- getW
  modifyW_ \w -> w { time = w.time + 1 }
  when (time `mod` speed == 0) $
    updateW_ { cells: Grid.modifyAllWithIndex (modifyCell cells time) cells }
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
