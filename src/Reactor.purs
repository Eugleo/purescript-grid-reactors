module Reactor
  ( runReactor
  , module Reactor.Action
  , module Colors
  , module Reactor.Graphics.CoordinateSystem
  , module Reactor.Graphics.Drawing
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
import Reactor.Graphics.Colors
  ( blue100
  , blue200
  , blue300
  , blue400
  , blue50
  , blue500
  , blue600
  , blue700
  , blue800
  , blue900
  , gray100
  , gray200
  , gray300
  , gray400
  , gray50
  , gray500
  , gray600
  , gray700
  , gray800
  , gray900
  , green100
  , green200
  , green300
  , green400
  , green50
  , green500
  , green600
  , green700
  , green800
  , green900
  , pink100
  , pink200
  , pink300
  , pink400
  , pink50
  , pink500
  , pink600
  , pink700
  , pink800
  , pink900
  , red100
  , red200
  , red300
  , red400
  , red50
  , red500
  , red600
  , red700
  , red800
  , red900
  , yellow100
  , yellow200
  , yellow300
  , yellow400
  , yellow50
  , yellow500
  , yellow600
  , yellow700
  , yellow800
  , yellow900
  ) as Colors
import Reactor.Graphics.CoordinateSystem (canvas, grid, wrt)
import Reactor.Graphics.Drawing (fill, cell)
import Reactor.Page (component) as Reactor.Page
import Reactor.Types (Reactor)

runReactor :: forall world. Reactor Aff { paused :: Boolean | world } -> Effect Unit
runReactor reactor =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (Reactor.Page.component reactor) unit body
