module Monadoc.Exception.MethodNotAllowed where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Vendor.HttpTypes as Http

data MethodNotAllowed
  = MethodNotAllowed Http.StdMethod Route.Route
  deriving (Eq, Show)

instance Exception.Exception MethodNotAllowed
