module Reactor.Internal.Widget where

import Prelude

data Ordering = First | Last

data Importance = Primary | Secondary | Tertiary

-- | Currently we support two types of widgets: sections and labels. Those two differ only in the way they are rendered. We recommend using labels for presenting important dynamic information to the user, and using sections to give short, descriptive labels to these pieces of information.
data Widget
  = Section { title :: String }
  -- | Button { importance :: Importance, label :: String, action :: world -> world }
  | Label { content :: String }