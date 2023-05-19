module Monadoc.Middleware.LogResponses where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Text
import qualified Data.Vault.Lazy as Vault
import qualified Formatting as F
import qualified GHC.Clock as Clock
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.RequestId as RequestId
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

middleware :: Context.Context -> Wai.Middleware
middleware context handle request respond = do
  before <- Clock.getMonotonicTime
  handle request $ \response -> do
    after <- Clock.getMonotonicTime
    let requestId = Maybe.fromMaybe RequestId.zero . Vault.lookup (Context.key context) $ Wai.vault request
    App.run context . App.Log.info $
      F.sformat
        ("[server]" F.%+ RequestId.format F.%+ F.int F.%+ F.stext F.%+ F.stext F.% F.stext F.%+ F.fixed 3)
        requestId
        (Http.statusCode $ Wai.responseStatus response)
        (Text.decodeUtf8Lenient $ Wai.requestMethod request)
        (Text.decodeUtf8Lenient $ Wai.rawPathInfo request)
        (Text.decodeUtf8Lenient $ Wai.rawQueryString request)
        (after - before)
    respond response
