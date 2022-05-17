module Monadoc.Server.Application where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Data.Bifunctor as Bifunctor
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Handler.AppleTouchIcon.Get as AppleTouchIcon.Get
import qualified Monadoc.Handler.Favicon.Get as Favicon.Get
import qualified Monadoc.Handler.HealthCheck.Get as HealthCheck.Get
import qualified Monadoc.Handler.Home.Get as Home.Get
import qualified Monadoc.Handler.Manifest.Get as Manifest.Get
import qualified Monadoc.Handler.Package.Get as Package.Get
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
  handler <- getHandler method route
  response <- Reader.runReaderT (App.runAppT $ handler request) context
  respond response

parseMethod :: Http.Method -> Either (Witch.TryFromException Http.Method Http.StdMethod) Http.StdMethod
parseMethod m = Bifunctor.first (const $ Witch.TryFromException m Nothing) $ Http.parseMethod m

getHandler ::
  Exception.MonadThrow m =>
  Http.StdMethod ->
  Route.Route ->
  m (Wai.Request -> App.App Wai.Response)
getHandler method route = case route of
  Route.AppleTouchIcon -> case method of
    Http.GET -> pure AppleTouchIcon.Get.handler
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
  Route.Package p -> case method of
    Http.GET -> pure $ Package.Get.handler p
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Robots -> case method of
    Http.GET -> pure Robots.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Script -> case method of
    Http.GET -> pure Script.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Search q -> case method of
    Http.GET -> pure $ Search.Get.handler q
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Stylesheet -> case method of
    Http.GET -> pure Stylesheet.Get.handler
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.User u -> case method of
    Http.GET -> pure $ User.Get.handler u
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
  Route.Version p v -> case method of
    Http.GET -> pure $ Version.Get.handler p v
    _ -> Exception.throwM $ MethodNotAllowed.MethodNotAllowed method route
