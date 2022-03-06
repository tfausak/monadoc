module Monadoc.Exception.Sick where

import qualified Control.Monad.Catch as Exception

data Sick
  = Sick
  deriving (Eq, Show)

instance Exception.Exception Sick
