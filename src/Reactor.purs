module Reactor
  ( runReactor
  , module Reactor.Action
  , module Reactor.Graphics.CoordinateSystem
  , module Reactor.Graphics.Drawing
  , module Reactor.Types
  , module Reactor.Page
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Reactor.Action
  ( executeDefaultBehavior
  , get
  , modify_
  , preventDefaultBehavior
  , togglePause
  , utilities
  )
import Reactor.Graphics.CoordinateSystem (canvas, grid, wrt)
import Reactor.Graphics.Drawing (fill, cell)
import Reactor.Page (component) as Reactor.Page
import Reactor.Types (Reactor)

runReactor :: forall world. Reactor Aff { paused :: Boolean | world } -> Effect Unit
runReactor reactor =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Reactor.Page.component reactor) unit body
