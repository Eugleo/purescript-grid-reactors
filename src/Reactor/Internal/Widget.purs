module Reactor.Internal.Widget where

import Prelude

data Ordering = First | Last

data Importance = Primary | Secondary | Tertiary

data Widget
  = Section { title :: String }
  -- | Button { importance :: Importance, label :: String, action :: world -> world }
  | Label { content :: String }