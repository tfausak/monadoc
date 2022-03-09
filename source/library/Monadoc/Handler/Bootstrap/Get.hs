module Monadoc.Handler.Bootstrap.Get where

import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath

handler :: Wai.Request -> App.App Wai.Response
handler _ = do
  context <- App.ask
  pure $
    Wai.responseFile
      Http.ok200
      [(Http.hContentType, ContentType.css)]
      (FilePath.combine (Context.data_ context) "bootstrap.css")
      Nothing
