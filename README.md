# Grid Reactors for Purescript

<img src='[figure/rstudio.png](https://github.com/Eugleo/purescript-grid-reactors/assets/21257632/7c148f50-9f3d-4135-9842-ddd09b4d4115)' width='200'>


Reactors are values enabling the creation of time-based animations, simulations, and interactive programs, as per [Pyret Documentation](https://www.pyret.org/docs/latest/reactors.html). This particular library brings the fun of reactors into Purescript, allowing you to create simple grid-based games in 3 steps:

1. Tell the reactor the initial state of your game, e.g. the positions of the player and the enemies.
2. Explain how a state can be drawn on a grid, using our intuitive DSL.
3. Finally, describe how the state changes depending on incoming mouse and keyboard input events, and if there is a background clock, how the state changes on the ticks.

Then you just call `runReactor [your reactor]` and you're all set! The reactor listens for the events, runs the clock, and does the rendering using a Halogen component. Declarative programming at its best.

## Example

If you thought we've skipped some details, you're wrong — that really was all there's to it. Let's see a simple example: a 'game' in which the user can move a blue dot using arrow keys.

```haskell
import Reactor.Action
  (executeDefaultBehavior, modify_, utilities)
import Reactor.Events (KeypressEvent(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.CoordinateSystem
  (CoordinateSystem, grid, moveDown, moveLeft, moveRight, moveUp, relativeTo)
import Reactor.Graphics.Drawing (fill, tile)
import Reactor.Types (Reactor)

type World =
  { player :: CoordinateSystem { x :: Number, y :: Number }
  , paused :: Boolean
  }

reactor :: forall m. Reactor m World
reactor =
  { init: { player: { x: 0, y: 0 } `relativeTo` grid, paused: true }
  , draw: \{ player } -> fill Color.blue400 $ tile player
  , handleEvent: \event -> do
      { bound } <- utilities
      case event of
        KeypressEvent "ArrowLeft" _ ->
          modify_ \w -> w { player = bound $ moveLeft w.player }
        KeypressEvent "ArrowRight" _ ->
          modify_ \w -> w { player = bound $ moveRight w.player }
        KeypressEvent "ArrowDown" _ ->
          modify_ \w -> w { player = bound $ moveDown w.player }
        KeypressEvent "ArrowUp" _ ->
          modify_ \w -> w { player = bound $ moveUp w.player }
        KeypressEvent " " _ -> togglePause
        _ -> executeDefaultBehavior
  }
```

We ignore the mouse events, and we pause the clock — the only way the state changes is through the keypress events. In the `draw` function, given a position of the player, we fill a blue tile on the grid in the same place.
