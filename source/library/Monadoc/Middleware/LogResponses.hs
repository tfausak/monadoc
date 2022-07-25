module Monadoc.Middleware.LogResponses where

import qualified Data.Text.Encoding as Text
import qualified Formatting as F
import qualified GHC.Clock as Clock
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

middleware :: Context.Context -> Wai.Middleware
middleware context handle request respond = do
  before <- Clock.getMonotonicTime
  handle request $ \response -> do
    after <- Clock.getMonotonicTime
    App.run context . App.Log.info $
      F.sformat
        (F.int F.% " " F.% F.stext F.% " " F.% F.stext F.% " " F.% F.fixed 3)
        (Http.statusCode $ Wai.responseStatus response)
        (Text.decodeUtf8Lenient $ Wai.requestMethod request)
        (Text.decodeUtf8Lenient $ Wai.rawPathInfo request)
        (after - before)
    respond response
