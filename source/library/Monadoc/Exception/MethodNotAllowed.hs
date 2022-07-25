module Monadoc.Exception.MethodNotAllowed where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http

data MethodNotAllowed
  = MethodNotAllowed Http.StdMethod Route.Route (Set.Set Http.StdMethod)
  deriving (Eq, Show)

instance Exception.Exception MethodNotAllowed

toAllowHeader :: Set.Set Http.StdMethod -> Http.Header
toAllowHeader =
  (,) Http.hAllow
    . ByteString.intercalate ", "
    . fmap Http.renderStdMethod
    . Set.toList
    . Set.insert Http.OPTIONS
