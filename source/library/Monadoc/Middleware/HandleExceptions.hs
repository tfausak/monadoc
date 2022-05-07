{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.HandleExceptions where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Exception.MethodNotAllowed as MethodNotAllowed
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Handler.Common as Common
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
  | isType @MethodNotAllowed.MethodNotAllowed e = Common.statusResponse Http.methodNotAllowed405 []
  | isType @NotFound.NotFound e = Common.statusResponse Http.notFound404 []
  | isType @UnknownRoute.UnknownRoute e = Common.statusResponse Http.notFound404 []
  | otherwise = Common.statusResponse Http.internalServerError500 []

isType :: forall e. Exception.Exception e => Exception.SomeException -> Bool
isType = Maybe.isJust . Exception.fromException @e
