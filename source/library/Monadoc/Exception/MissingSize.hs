module Monadoc.Exception.MissingSize where

import qualified Control.Monad.Catch as Exception
import qualified Network.HTTP.Client as Client

newtype MissingSize
  = MissingSize (Client.Response ())
  deriving (Show)

instance Exception.Exception MissingSize
