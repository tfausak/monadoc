{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Handler.Common as Handler
import qualified Monadoc.Template.Common as Template
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
  MonadLog.MonadLog m =>
  Context.Context ->
  (Wai.Response -> m Wai.ResponseReceived) ->
  Exception.SomeException ->
  m Wai.ResponseReceived
handler context respond someException = do
  onException someException
  respond $ onExceptionResponse context someException

onException :: MonadLog.MonadLog m => Exception.SomeException -> m ()
onException = MonadLog.error . Text.pack . Exception.displayException

onExceptionResponse :: Context.Context -> Exception.SomeException -> Wai.Response
onExceptionResponse context e
  | Just (Found.Found route) <- Exception.fromException e =
      Handler.statusResponse Http.found302 [(Http.hLocation, Witch.into @ByteString.ByteString $ Template.route context route)]
  | isType @MethodNotAllowed.MethodNotAllowed e =
      Handler.statusResponse Http.methodNotAllowed405 []
  | isType @NotFound.NotFound e =
      Handler.statusResponse Http.notFound404 []
  | isType @UnknownRoute.UnknownRoute e =
      Handler.statusResponse Http.notFound404 []
  | otherwise =
      Handler.statusResponse Http.internalServerError500 []

isType :: forall e. Exception.Exception e => Exception.SomeException -> Bool
isType = Maybe.isJust . Exception.fromException @e
