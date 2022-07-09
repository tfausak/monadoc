module Monadoc.Handler.Favicon.Get where

import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Type.App as App
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Wai.Request -> App.App Wai.Response
handler _ =
  Common.fileResponse
    Http.ok200
    [(Http.hContentType, ContentType.ico)]
    "favicon.ico"
