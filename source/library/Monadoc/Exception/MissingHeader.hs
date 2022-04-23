module Monadoc.Exception.MissingHeader where

import qualified Control.Monad.Catch as Exception
import qualified Network.HTTP.Types as Http

newtype MissingHeader
  = MissingHeader Http.HeaderName
  deriving (Show)

instance Exception.Exception MissingHeader
