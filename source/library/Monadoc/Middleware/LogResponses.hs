module Monadoc.Middleware.LogResponses where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Numeric

middleware :: Wai.Middleware
middleware handle request respond = do
  before <- MonadTime.getMonotonicTime
  handle request $ \response -> do
    after <- MonadTime.getMonotonicTime
    MonadLog.info $
      Text.unwords
        [ Text.pack . show . Http.statusCode $ Wai.responseStatus response,
          Text.decodeUtf8Lenient $ Wai.requestMethod request,
          Text.decodeUtf8Lenient $ Wai.rawPathInfo request,
          Text.pack $ Numeric.showFFloat (Just 3) (after - before) ""
        ]
    respond response
