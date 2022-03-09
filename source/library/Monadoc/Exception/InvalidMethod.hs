module Monadoc.Exception.InvalidMethod where

import qualified Control.Monad.Catch as Exception
import qualified Network.HTTP.Types as Http

newtype InvalidMethod
  = InvalidMethod Http.Method
  deriving (Eq, Show)

instance Exception.Exception InvalidMethod
