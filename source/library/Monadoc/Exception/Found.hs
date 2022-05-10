module Monadoc.Exception.Found where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Route as Route

newtype Found
  = Found Route.Route
  deriving (Eq, Show)

instance Exception.Exception Found
