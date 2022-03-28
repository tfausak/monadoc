{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

middleware :: Wai.Middleware
middleware handle request respond =
  Exception.handle (handler respond)
    . handle request
    $ respond

handler ::
  MonadLog.MonadLog m =>
  (Wai.Response -> m Wai.ResponseReceived) ->
  Exception.SomeException ->
  m Wai.ResponseReceived
handler respond someException = do
  onException someException
  respond $ onExceptionResponse someException

onException :: MonadLog.MonadLog m => Exception.SomeException -> m ()
onException someException =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . MonadLog.error
    . Text.pack
    $ Exception.displayException someException

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse e
  | isType @UnknownRoute.UnknownRoute e = statusResponse Http.notFound404 []
  | isType @MethodNotAllowed.MethodNotAllowed e = statusResponse Http.methodNotAllowed405 []
  | otherwise = statusResponse Http.internalServerError500 []

isType :: forall e. Exception.Exception e => Exception.SomeException -> Bool
isType = Maybe.isJust . Exception.fromException @e

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.text) : headers)
    . Witch.into @LazyByteString.ByteString
    $ (Witch.into @ByteString.ByteString $ show (Http.statusCode status) <> " ")
      <> Http.statusMessage status
