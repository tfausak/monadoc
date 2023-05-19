module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Vault.Lazy as Vault
import qualified Monadoc.Action.Exception.Log as Exception.Log
import qualified Monadoc.Action.Exception.NotifySentry as Exception.NotifySentry
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.TimedOut as TimedOut
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Extra.Exception as Exception
import qualified Monadoc.Handler.Common as Handler
import qualified Monadoc.Template.Common as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Patrol
import qualified Patrol.Type.Event as Patrol.Event
import qualified Patrol.Type.Request as Patrol.Request
import qualified Witch

middleware :: Context.Context -> Wai.Middleware
middleware context handle request respond =
  Exception.handle (handler context request respond) $
    handle request respond

handler ::
  Context.Context ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  Exception.SomeException ->
  IO Wai.ResponseReceived
handler context request respond exception = do
  onException context (Just request) exception
  respond $ onExceptionResponse context exception

onException ::
  Context.Context ->
  Maybe Wai.Request ->
  Exception.SomeException ->
  IO ()
onException context maybeRequest exception = App.run context $ do
  Exception.Log.run exception
  Exception.NotifySentry.run (maybe id (withRequest context) maybeRequest) exception

withRequest :: Context.Context -> Wai.Request -> Patrol.Event -> Patrol.Event
withRequest context request event =
  let oldRequest = Maybe.fromMaybe Patrol.Request.empty $ Patrol.Event.request event
      newRequest =
        oldRequest
          { Patrol.Request.env = Map.insert "MONADOC_REQUEST_ID" (Aeson.toJSON . Vault.lookup (Context.key context) $ Wai.vault request) $ Patrol.Request.env oldRequest,
            Patrol.Request.headers =
              Map.fromList
                . fmap (Bifunctor.bimap (fromUtf8 . CI.foldedCase) fromUtf8)
                $ Wai.requestHeaders request,
            Patrol.Request.method = fromUtf8 $ Wai.requestMethod request,
            Patrol.Request.queryString =
              fmap (Maybe.fromMaybe Text.empty)
                . Map.fromList
                . Http.parseQueryText
                $ Wai.rawQueryString request,
            Patrol.Request.url =
              (Text.pack . Config.base $ Context.config context)
                <> (Text.drop 1 . fromUtf8 $ Wai.rawPathInfo request)
          }
   in event {Patrol.Event.request = Just newRequest}

fromUtf8 :: ByteString.ByteString -> Text.Text
fromUtf8 = Text.decodeUtf8With Text.lenientDecode

onExceptionResponse :: Context.Context -> Exception.SomeException -> Wai.Response
onExceptionResponse context e
  | Just (Traced.Traced e2 _) <- Exception.fromException e = onExceptionResponse context e2
  | Just (Found.Found route) <- Exception.fromException e =
      Handler.statusResponse Http.found302 [(Http.hLocation, Witch.via @(Witch.UTF_8 ByteString.ByteString) $ Template.route context route)]
  | Just (MethodNotAllowed.MethodNotAllowed _ _ ms) <- Exception.fromException e =
      Handler.statusResponse Http.methodNotAllowed405 [MethodNotAllowed.toAllowHeader ms]
  | Exception.isType @NotFound.NotFound e =
      Handler.statusResponse Http.notFound404 []
  | Exception.isType @UnknownRoute.UnknownRoute e =
      Handler.statusResponse Http.notFound404 []
  | Exception.isType @TimedOut.TimedOut e =
      Handler.statusResponse Http.serviceUnavailable503 []
  | otherwise =
      Handler.statusResponse Http.internalServerError500 []
