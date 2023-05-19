module Monadoc.Exception.TimedOut where

import qualified Control.Monad.Catch as Exception

data TimedOut
  = TimedOut
  deriving (Eq, Show)

instance Exception.Exception TimedOut
