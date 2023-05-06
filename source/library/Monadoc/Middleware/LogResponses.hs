module Monadoc.Middleware.LogResponses where

import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Formatting as F
import qualified GHC.Clock as Clock
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.Random as Random

middleware :: Context.Context -> Wai.Middleware
middleware context handle request respond = do
  before <- Clock.getMonotonicTime
  requestId <- Random.randomIO @Word.Word32
  App.run context . App.Log.info $
    F.sformat
      ("[http/server/request]" F.%+ F.hexPrefix 8 F.%+ F.stext F.%+ F.stext F.% F.stext)
      requestId
      (Text.decodeUtf8Lenient $ Wai.requestMethod request)
      (Text.decodeUtf8Lenient $ Wai.rawPathInfo request)
      (Text.decodeUtf8Lenient $ Wai.rawQueryString request)
  handle request $ \response -> do
    after <- Clock.getMonotonicTime
    App.run context . App.Log.info $
      F.sformat
        ("[http/server/response]" F.%+ F.hexPrefix 8 F.%+ F.int F.%+ F.fixed 3)
        requestId
        (Http.statusCode $ Wai.responseStatus response)
        (after - before)
    respond response
