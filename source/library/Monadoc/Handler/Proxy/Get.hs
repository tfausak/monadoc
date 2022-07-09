{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Handler.Proxy.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Url as Url
import qualified Network.HTTP.Client as Client
import qualified Network.Wai as Wai
import qualified Witch

handler ::
  Context.Context ->
  Hash.Hash ->
  Url.Url ->
  Wai.Request ->
  App.App Wai.Response
handler context actual url _ = do
  let expected = makeHash context url
  Monad.when (actual /= expected) . Traced.throw $
    Mismatch.Mismatch
      { Mismatch.expected = expected,
        Mismatch.actual = actual
      }
  request <- Client.requestFromURI $ Witch.from url
  Control.control $ \runInBase -> Client.withResponse request {Client.checkResponse = Client.throwErrorStatusCodes} (Context.manager context) $ \response -> runInBase $ do
    -- TODO: Stream response body.
    chunks <- IO.liftIO . Client.brConsume $ Client.responseBody response
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
