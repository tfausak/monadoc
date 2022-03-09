module Monadoc.Server.Application where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Exception.InvalidMethod as InvalidMethod
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Handler.AppleTouchIcon.Get as AppleTouchIcon.Get
import qualified Monadoc.Handler.Bootstrap.Get as Bootstrap.Get
import qualified Monadoc.Handler.Favicon.Get as Favicon.Get
import qualified Monadoc.Handler.HealthCheck.Get as HealthCheck.Get
import qualified Monadoc.Handler.Home.Get as Home.Get
import qualified Monadoc.Handler.Manifest.Get as Manifest.Get
import qualified Monadoc.Handler.Robots.Get as Robots.Get
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

application :: Context.Context -> Wai.Application
application context request respond = do
  method <-
    either (Exception.throwM . InvalidMethod.InvalidMethod) pure
      . Http.parseMethod
      $ Wai.requestMethod request
  route <- Route.parse $ Wai.pathInfo request
  handler <- getHandler method route
  response <- App.run (handler request) context
  respond response

getHandler ::
  Exception.MonadThrow m =>
  Http.StdMethod ->
  Route.Route ->
  m (Wai.Request -> App.App Wai.Response)
getHandler method route = case route of
  Route.AppleTouchIcon -> case method of
    Http.GET -> pure AppleTouchIcon.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Bootstrap -> case method of
    Http.GET -> pure Bootstrap.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Favicon -> case method of
    Http.GET -> pure Favicon.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.HealthCheck -> case method of
    Http.GET -> pure HealthCheck.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Home -> case method of
    Http.GET -> pure Home.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Manifest -> case method of
    Http.GET -> pure Manifest.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Robots -> case method of
    Http.GET -> pure Robots.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
