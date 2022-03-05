module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Say as Say

middleware :: Wai.Middleware
middleware handle request respond =
  Exception.handle
    ( \someException -> do
        onException someException
        respond onExceptionResponse
    )
    . handle request
    $ respond

onException :: Exception.SomeException -> IO ()
onException someException =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . Say.sayErrString
    $ Exception.displayException someException

onExceptionResponse :: Wai.Response
onExceptionResponse =
  Wai.responseLBS Http.internalServerError500 [] LazyByteString.empty
