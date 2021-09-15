# Grid Reactors for Purescript

Reactors are values enabling the creation of time-based animations, simulations, and interactive programs, as per [Pyret Documentation](https://www.pyret.org/docs/latest/reactors.html). This particular library brings the fun of reactors into Purescript, allowing you to create simple grid-based games in 3 steps:

1. Tell the reactor the initial state of your game, e.g. the positions of the player and the enemies.
2. Explain how a state can be drawn on a grid, using our intuitive DSL.
3. Finally, describe how the state changes depending on incoming mouse and keyboard input events, and if there is a background clock, how the state changes on the ticks.

Then you just call `runReactor [your reactor]` and you're set! The reactor listens for the events, runs the clock, and does the rendering using a Halogen component. Declarative programming at its best.

## Example

If you thought we've skipped some details, you're wrong — that really was all there's to it. Let's see a simple example: a 'game' in which the user can move a blue dot using arrow keys.

```haskell
import Reactor.Action
  (executeDefaultBehavior, modify_, preventDefaultBehavior, utilities)
import Reactor.Events (KeypressEvent(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.CoordinateSystem
  (CoordinateSystem, grid, moveDown, moveLeft, moveRight, moveUp, wrt)
import Reactor.Graphics.Drawing (fill, tile)
import Reactor.Types (Reactor)

type World =
  { player :: CoordinateSystem { x :: Number, y :: Number }
  , paused :: Boolean
  }

reactor :: forall m. Reactor m World
reactor =
  { init: { dot: { x: 0, y: 0 } `wrt` grid, paused: true }
  , draw: \{ dot } -> fill Color.blue400 $ tile dot
  , onTick: \_ -> pure unit
  , onKey: \(KeypressEvent key _) -> do
      { bound } <- utilities
      case key of
        "ArrowLeft" -> do
          modify_ \w@{ player } ->
            w { player = bound $ moveLeft player }
          preventDefaultBehavior
        "ArrowRight" -> do
          modify_ \w@{ player } ->
            w { player = bound $ moveRight player }
          preventDefaultBehavior
        "ArrowDown" -> do
          modify_ \w@{ player } ->
            w { player = bound $ moveDown player }
          preventDefaultBehavior
        "ArrowUp" -> do
          modify_ \w@{ player } ->
            w { player = bound $ moveUp player }
          preventDefaultBehavior
        _ -> executeDefaultBehavior
    , onMouse: \_ -> executeDefaultBehavior
  }
```

We ignore the mouse events, and we pause the clock — the only way the state changes is through the `onKey` event handler. In the `draw` function, given a position of a dot, we fill a blue tile on the grid in the same place.
