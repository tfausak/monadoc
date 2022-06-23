{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Handler.Proxy.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Url as Url
import qualified Network.HTTP.Client as Client
import qualified Network.Wai as Wai
import qualified Witch

handler ::
  (Base.MonadBase IO m, MonadHttp.MonadHttp m, Exception.MonadThrow m) =>
  Context.Context ->
  Hash.Hash ->
  Url.Url ->
  Wai.Request ->
  m Wai.Response
handler context actual url _ = do
  let expected = makeHash context url
  Monad.when (actual /= expected) . Exception.throwM $
    Mismatch.Mismatch
      { Mismatch.expected = expected,
        Mismatch.actual = actual
      }
  request <- Client.requestFromURI $ Witch.from url
  MonadHttp.withResponse (request {Client.checkResponse = Client.throwErrorStatusCodes}) $ \response -> do
    -- TODO: Stream response body.
    chunks <- Base.liftBase . Client.brConsume $ Client.responseBody response
    pure
      . Wai.responseLBS (Client.responseStatus response) (Client.responseHeaders response)
      $ LazyByteString.fromChunks chunks

makeRoute :: Context.Context -> Url.Url -> Route.Route
makeRoute context url = Route.Proxy (makeHash context url) url

makeHash :: Context.Context -> Url.Url -> Hash.Hash
makeHash context =
  Hash.new
    . Witch.from
    . mappend (Config.salt $ Context.config context)
    . Witch.from
