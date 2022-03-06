module Monadoc.Handler.AppleTouchIcon.Get where

import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.HttpTypes as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath

handler :: Wai.Request -> App.App Wai.Response
handler _ = do
  context <- App.ask
  pure $
    Wai.responseFile
      Http.ok200
      [(Http.hContentType, ContentType.png)]
      (FilePath.combine (Context.dataDirectory context) "apple-touch-icon.png")
      Nothing
