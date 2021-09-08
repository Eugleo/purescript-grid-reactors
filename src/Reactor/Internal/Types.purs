module Reactor.Internal.Types where

import Prelude
import Color (Color)

data Cell = Colored Color | EmptyCell
derive instance eqCell :: Eq Cell