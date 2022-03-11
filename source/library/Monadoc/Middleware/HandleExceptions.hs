{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Exception.InvalidMethod as InvalidMethod
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Say
import qualified Witch

middleware :: Wai.Middleware
middleware handle request respond =
  Exception.handle (handler respond)
    . handle request
    $ respond

handler ::
  (Wai.Response -> IO Wai.ResponseReceived) ->
  Exception.SomeException ->
  IO Wai.ResponseReceived
handler respond someException = do
  onException someException
  respond $ onExceptionResponse someException

onException :: Exception.SomeException -> IO ()
onException someException =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . Say.sayErrString
    $ Exception.displayException someException

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse e
  | isType @InvalidMethod.InvalidMethod e = statusResponse Http.methodNotAllowed405 []
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
