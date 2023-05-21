module Monadoc.Handler.Proxy.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.HttpClient as Client
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Url as Url
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
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
    -- TODO: Forward headers from the client?
    Client.withResponse
      (Client.ensureUserAgent $ Client.setRequestCheckStatus request)
      (Context.manager context)
      $ \response -> runInBase $ do
        logResponse response
        let headers = filter (flip Set.member headersToKeep . fst) $ Client.responseHeaders response
        respond
          . Wai.responseStream (Client.responseStatus response) headers
          $ \send flush -> do
            Loops.whileJust_ (readChunk response) $ send . Builder.byteString
            flush

headersToKeep :: Set.Set Http.HeaderName
headersToKeep =
  Set.fromList
    [ Http.hCacheControl,
      Http.hContentEncoding,
      Http.hContentLength,
      Http.hContentType,
      Http.hETag,
      Http.hExpires,
      Http.hLastModified,
      Http.hTransferEncoding
    ]

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

logResponse :: Client.Response a -> App.App ()
logResponse response =
  App.Log.debug $
    F.sformat
      ("[client]" F.%+ F.int F.%+ F.stext F.%+ F.stext F.%+ F.stext F.%+ F.stext)
      (Http.statusCode $ Client.responseStatus response)
      (Text.decodeUtf8Lenient . Client.method $ Client.getOriginalRequest response)
      (Text.decodeUtf8Lenient . Client.host $ Client.getOriginalRequest response)
      (Text.decodeUtf8Lenient . Client.path $ Client.getOriginalRequest response)
      (Text.decodeUtf8Lenient . Client.queryString $ Client.getOriginalRequest response)
