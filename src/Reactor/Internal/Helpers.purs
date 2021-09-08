module Reactor.Internal.Helpers (withJust) where

import Prelude

import Data.Maybe (Maybe(..))

withJust :: forall a b m. Applicative m => Maybe a -> (a -> m b) -> m Unit
withJust (Just x) f = f x *> pure unit
withJust Nothing _ = pure unit