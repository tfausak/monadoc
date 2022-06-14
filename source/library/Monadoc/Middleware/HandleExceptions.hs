{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
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
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

middleware :: Context.Context -> Wai.Middleware
middleware context handle request respond =
  Exception.handle (handler context respond)
    . handle request
    $ respond

handler ::
  (Control.MonadBaseControl IO m, MonadLog.MonadLog m) =>
  Context.Context ->
  (Wai.Response -> m Wai.ResponseReceived) ->
  Exception.SomeException ->
  m Wai.ResponseReceived
handler context respond exception = do
  onException context exception
  respond $ onExceptionResponse context exception

onException ::
  (Control.MonadBaseControl IO m, MonadLog.MonadLog m) =>
  Context.Context ->
  Exception.SomeException ->
  m ()
onException context exception = do
  Exception.Log.run exception
  Reader.runReaderT (App.runAppT $ Exception.NotifySentry.run exception) context

onExceptionResponse :: Context.Context -> Exception.SomeException -> Wai.Response
onExceptionResponse context e
  | Just (Found.Found route) <- Exception.fromException e =
      Handler.statusResponse Http.found302 [(Http.hLocation, Witch.into @ByteString.ByteString $ Template.route context route)]
  | Exception.isType @MethodNotAllowed.MethodNotAllowed e =
      Handler.statusResponse Http.methodNotAllowed405 []
  | Exception.isType @NotFound.NotFound e =
      Handler.statusResponse Http.notFound404 []
  | Exception.isType @UnknownRoute.UnknownRoute e =
      Handler.statusResponse Http.notFound404 []
  | otherwise =
      Handler.statusResponse Http.internalServerError500 []
