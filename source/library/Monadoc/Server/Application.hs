module Monadoc.Server.Application where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Handler.Component.Get as Component.Get
import qualified Monadoc.Handler.HealthCheck.Get as HealthCheck.Get
import qualified Monadoc.Handler.Home.Get as Home.Get
import qualified Monadoc.Handler.Manifest.Get as Manifest.Get
import qualified Monadoc.Handler.Module.Get as Module.Get
import qualified Monadoc.Handler.Package.Get as Package.Get
import qualified Monadoc.Handler.Proxy.Get as Proxy.Get
import qualified Monadoc.Handler.Robots.Get as Robots.Get
import qualified Monadoc.Handler.Search.Get as Search.Get
import qualified Monadoc.Handler.User.Get as User.Get
import qualified Monadoc.Handler.Version.Get as Version.Get
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
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
  route <- NotFound.fromMaybe $ Route.parse (Wai.pathInfo request) (Wai.queryString request)
  handler <- getHandler context method route
  App.run context . handler request $ IO.liftIO . respond

parseMethod :: Http.Method -> Either (Witch.TryFromException Http.Method Http.StdMethod) Http.StdMethod
parseMethod m = Bifunctor.first (const $ Witch.TryFromException m Nothing) $ Http.parseMethod m

getHandler ::
  (Exception.MonadThrow m) =>
  Context.Context ->
  Http.StdMethod ->
  Route.Route ->
  m Handler.Handler
getHandler context method route = case route of
  -- TODO: Implement better handling for static routes.
  Route.AppleTouchIcon -> Exception.throwM NotFound.NotFound
  Route.Component p v c -> resource method route . Map.singleton Http.GET $ Component.Get.handler p v c
  Route.Favicon -> Exception.throwM NotFound.NotFound
  Route.HealthCheck -> resource method route $ Map.singleton Http.GET HealthCheck.Get.handler
  Route.Home -> resource method route $ Map.singleton Http.GET Home.Get.handler
  Route.Manifest -> resource method route $ Map.singleton Http.GET Manifest.Get.handler
  Route.Module p v c m -> resource method route . Map.singleton Http.GET $ Module.Get.handler p v c m
  Route.Package p -> resource method route . Map.singleton Http.GET $ Package.Get.handler p
  Route.Proxy h u -> resource method route . Map.singleton Http.GET $ Proxy.Get.handler context h u
  Route.Robots -> resource method route $ Map.singleton Http.GET Robots.Get.handler
  Route.Script -> Exception.throwM NotFound.NotFound
  Route.Search q -> resource method route . Map.singleton Http.GET $ Search.Get.handler q
  Route.Stylesheet -> Exception.throwM NotFound.NotFound
  Route.User u -> resource method route . Map.singleton Http.GET $ User.Get.handler u
  Route.Version p v -> resource method route . Map.singleton Http.GET $ Version.Get.handler p v

resource ::
  (Exception.MonadThrow m) =>
  Http.StdMethod ->
  Route.Route ->
  Map.Map Http.StdMethod Handler.Handler ->
  m Handler.Handler
resource method route m =
  if method == Http.OPTIONS
    then pure $ \_ respond ->
      respond
        . Common.statusResponse Http.ok200
        . pure
        . MethodNotAllowed.toAllowHeader
        $ Map.keysSet m
    else case Map.lookup method m of
      Nothing -> Traced.throw . MethodNotAllowed.MethodNotAllowed method route $ Map.keysSet m
      Just f -> pure f
