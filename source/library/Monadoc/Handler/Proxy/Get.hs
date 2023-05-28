module Monadoc.Handler.Proxy.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.HttpClient as Client
import qualified Monadoc.Middleware.AddHeaders as AddHeaders
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
handler context actual url request respond = do
  let expected = makeHash context url
  Monad.when (actual /= expected) . Traced.throw $
    Mismatch.Mismatch
      { Mismatch.expected = expected,
        Mismatch.actual = actual
      }
  proxy <- Client.requestFromURI $ Witch.from url
  Control.control $ \runInBase ->
    Client.withResponse
      (forwardHeaders request . Client.ensureUserAgent $ Client.setRequestCheckStatus proxy)
      (Context.manager context)
      $ \response -> runInBase $ do
        logResponse response
        let headers = filter (flip Set.member headersToKeep . fst) $ Client.responseHeaders response
        respond
          . Wai.responseStream (Client.responseStatus response) headers
          $ \send flush -> do
            Loops.whileJust_ (readChunk response) $ send . Builder.byteString
            flush

forwardHeaders :: Wai.Request -> Client.Request -> Client.Request
forwardHeaders request proxy =
  let getHeader name = fmap ((,) name) . lookup name $ Wai.requestHeaders request
      forwarded = Maybe.mapMaybe getHeader [Http.hAccept, Http.hAcceptEncoding]
   in proxy {Client.requestHeaders = AddHeaders.addHeaders forwarded $ Client.requestHeaders proxy}

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
logResponse response = do
  let request = Client.getOriginalRequest response
  App.Log.debug $
    F.sformat
      ("[client]" F.%+ F.int F.%+ F.stext F.%+ F.stext F.% F.stext F.% F.stext F.% F.stext)
      (Http.statusCode $ Client.responseStatus response)
      (Text.decodeUtf8Lenient $ Client.method request)
      (if Client.secure request then "https://" else "http://")
      (Text.decodeUtf8Lenient $ Client.host request)
      (Text.decodeUtf8Lenient $ Client.path request)
      (Text.decodeUtf8Lenient $ Client.queryString request)
