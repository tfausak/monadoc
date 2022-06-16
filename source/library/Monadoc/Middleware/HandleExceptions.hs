{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Monadoc.Action.Exception.Log as Exception.Log
import qualified Monadoc.Action.Exception.NotifySentry as Exception.NotifySentry
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.NotFound as NotFound
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
  Exception.handle (handler context request respond)
    . handle request
    $ respond

handler ::
  (Control.MonadBaseControl IO m, MonadLog.MonadLog m) =>
  Context.Context ->
  Wai.Request ->
  (Wai.Response -> m Wai.ResponseReceived) ->
  Exception.SomeException ->
  m Wai.ResponseReceived
handler context request respond exception = do
  onException context (Just request) exception
  respond $ onExceptionResponse context exception

onException ::
  (Control.MonadBaseControl IO m, MonadLog.MonadLog m) =>
  Context.Context ->
  Maybe Wai.Request ->
  Exception.SomeException ->
  m ()
onException context maybeRequest exception = do
  Exception.Log.run exception
  Reader.runReaderT (App.runAppT $ Exception.NotifySentry.run (maybe id (withRequest context) maybeRequest) exception) context

withRequest :: Context.Context -> Wai.Request -> Patrol.Event -> Patrol.Event
withRequest context request event =
  event
    { Patrol.Event.request =
        Just
          Patrol.Request.Request
            { Patrol.Request.cookies = Nothing,
              Patrol.Request.data_ = Nothing,
              Patrol.Request.env = Nothing,
              Patrol.Request.headers = Just . Map.fromList . fmap (Bifunctor.bimap (fromUtf8 . CI.foldedCase) fromUtf8) $ Wai.requestHeaders request,
              Patrol.Request.method = Just . fromUtf8 $ Wai.requestMethod request,
              Patrol.Request.queryString = Just . fmap (Maybe.fromMaybe Text.empty) . Map.fromList . Http.parseQueryText $ Wai.rawQueryString request,
              Patrol.Request.url = Just $ (Text.pack . Config.base $ Context.config context) <> fromUtf8 (Wai.rawPathInfo request)
            }
    }

fromUtf8 :: ByteString.ByteString -> Text.Text
fromUtf8 = Text.decodeUtf8With Text.lenientDecode

onExceptionResponse :: Context.Context -> Exception.SomeException -> Wai.Response
onExceptionResponse context e
  | Just (Found.Found route) <- Exception.fromException e =
      Handler.statusResponse Http.found302 [(Http.hLocation, Witch.into @ByteString.ByteString $ Template.route context route)]
  | Just (MethodNotAllowed.MethodNotAllowed _ _ ms) <- Exception.fromException e =
      Handler.statusResponse Http.methodNotAllowed405 [MethodNotAllowed.toAllowHeader ms]
  | Exception.isType @NotFound.NotFound e =
      Handler.statusResponse Http.notFound404 []
  | Exception.isType @UnknownRoute.UnknownRoute e =
      Handler.statusResponse Http.notFound404 []
  | otherwise =
      Handler.statusResponse Http.internalServerError500 []
