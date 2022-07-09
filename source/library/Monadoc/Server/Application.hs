module Monadoc.Server.Application where

import qualified Control.Monad.Catch as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Handler.AppleTouchIcon.Get as AppleTouchIcon.Get
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Handler.Favicon.Get as Favicon.Get
import qualified Monadoc.Handler.HealthCheck.Get as HealthCheck.Get
import qualified Monadoc.Handler.Home.Get as Home.Get
import qualified Monadoc.Handler.Manifest.Get as Manifest.Get
import qualified Monadoc.Handler.Package.Get as Package.Get
import qualified Monadoc.Handler.Proxy.Get as Proxy.Get
import qualified Monadoc.Handler.Robots.Get as Robots.Get
import qualified Monadoc.Handler.Script.Get as Script.Get
import qualified Monadoc.Handler.Search.Get as Search.Get
import qualified Monadoc.Handler.Stylesheet.Get as Stylesheet.Get
import qualified Monadoc.Handler.User.Get as User.Get
import qualified Monadoc.Handler.Version.Get as Version.Get
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

application :: Context.Context -> Wai.Application
application context request respond = do
  method <-
    Either.throw
      . parseMethod
      $ Wai.requestMethod request
  route <- Route.parse (Wai.pathInfo request) (Wai.queryString request)
  handler <- getHandler context method route
  response <- App.runApp context $ handler request
  respond response

parseMethod :: Http.Method -> Either (Witch.TryFromException Http.Method Http.StdMethod) Http.StdMethod
parseMethod m = Bifunctor.first (const $ Witch.TryFromException m Nothing) $ Http.parseMethod m

getHandler ::
  Exception.MonadThrow m =>
  Context.Context ->
  Http.StdMethod ->
  Route.Route ->
  m (Wai.Request -> App.App Wai.Response)
getHandler context method route = case route of
  Route.AppleTouchIcon -> resource method route $ Map.singleton Http.GET AppleTouchIcon.Get.handler
  Route.Favicon -> resource method route $ Map.singleton Http.GET Favicon.Get.handler
  Route.HealthCheck -> resource method route $ Map.singleton Http.GET HealthCheck.Get.handler
  Route.Home -> resource method route $ Map.singleton Http.GET Home.Get.handler
  Route.Manifest -> resource method route $ Map.singleton Http.GET Manifest.Get.handler
  Route.Package p -> resource method route . Map.singleton Http.GET $ Package.Get.handler p
  Route.Proxy h u -> resource method route . Map.singleton Http.GET $ Proxy.Get.handler context h u
  Route.Robots -> resource method route $ Map.singleton Http.GET Robots.Get.handler
  Route.Script -> resource method route $ Map.singleton Http.GET Script.Get.handler
  Route.Search q -> resource method route . Map.singleton Http.GET $ Search.Get.handler q
  Route.Stylesheet -> resource method route $ Map.singleton Http.GET Stylesheet.Get.handler
  Route.User u -> resource method route . Map.singleton Http.GET $ User.Get.handler u
  Route.Version p v -> resource method route . Map.singleton Http.GET $ Version.Get.handler p v

resource ::
  Exception.MonadThrow m =>
  Http.StdMethod ->
  Route.Route ->
  Map.Map Http.StdMethod (Wai.Request -> App.App Wai.Response) ->
  m (Wai.Request -> App.App Wai.Response)
resource method route m =
  if method == Http.OPTIONS
    then
      pure
        . const
        . pure
        . Common.statusResponse Http.ok200
        . pure
        . MethodNotAllowed.toAllowHeader
        $ Map.keysSet m
    else case Map.lookup method m of
      Nothing -> Traced.throw . MethodNotAllowed.MethodNotAllowed method route $ Map.keysSet m
      Just f -> pure f
