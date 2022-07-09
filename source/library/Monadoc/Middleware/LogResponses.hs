module Monadoc.Middleware.LogResponses where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Clock as Clock
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Numeric

middleware :: Context.Context -> Wai.Middleware
middleware context handle request respond = do
  before <- Clock.getMonotonicTime
  handle request $ \response -> do
    after <- Clock.getMonotonicTime
    App.runApp context . Log.info $
      Text.unwords
        [ Text.pack . show . Http.statusCode $ Wai.responseStatus response,
          Text.decodeUtf8Lenient $ Wai.requestMethod request,
          Text.decodeUtf8Lenient $ Wai.rawPathInfo request,
          Text.pack $ Numeric.showFFloat (Just 3) (after - before) ""
        ]
    respond response
