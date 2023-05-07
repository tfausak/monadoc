module Monadoc.Handler.Proxy.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.HttpClient as Client
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
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
  Handler.Handler
handler context actual url _ respond = do
  let expected = makeHash context url
  Monad.when (actual /= expected) . Traced.throw $
    Mismatch.Mismatch
      { Mismatch.expected = expected,
        Mismatch.actual = actual
      }
  request <- Client.requestFromURI $ Witch.from url
  Control.control $ \runInBase ->
    Client.withResponse (Client.ensureUserAgent request) {Client.checkResponse = Client.throwErrorStatusCodes} (Context.manager context) $ \response ->
      -- TODO: Limit which headers are forwarded?
      runInBase . respond . Wai.responseStream (Client.responseStatus response) (Client.responseHeaders response) $ \send flush ->
        Loops.whileJust_ (readChunk response) $ \chunk -> do
          send $ Builder.byteString chunk
          flush

readChunk :: Client.Response Client.BodyReader -> IO (Maybe ByteString.ByteString)
readChunk response = do
  chunk <- Client.brRead $ Client.responseBody response
  pure $ if ByteString.null chunk then Nothing else Just chunk

makeRoute :: Context.Context -> Url.Url -> Route.Route
makeRoute context url = Route.Proxy (makeHash context url) url

makeHash :: Context.Context -> Url.Url -> Hash.Hash
makeHash context =
  Hash.new
    . Witch.via @(Witch.UTF_8 ByteString.ByteString)
    . mappend (Config.salt $ Context.config context)
    . Witch.from
