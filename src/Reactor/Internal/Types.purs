module Reactor.Internal.Types (Cell(..)) where

import Prelude
import Color (Color)

data Cell = Colored Color | EmptyCell
derive instance eqCell :: Eq Cell