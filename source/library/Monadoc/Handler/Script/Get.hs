module Monadoc.Handler.Script.Get where

import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Type.Handler as Handler
import qualified Network.HTTP.Types as Http

handler :: Handler.Handler
handler _ respond = do
  response <-
    Common.fileResponse
      Http.ok200
      [(Http.hContentType, ContentType.js)]
      "monadoc.js"
  respond response
