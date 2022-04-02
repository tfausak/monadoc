module Monadoc.Handler.Favicon.Get where

import qualified Control.Monad.Reader as Reader
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath

handler :: Reader.MonadReader Context.Context m => Wai.Request -> m Wai.Response
handler _ = do
  context <- Reader.ask
  pure $
    Wai.responseFile
      Http.ok200
      [(Http.hContentType, ContentType.ico)]
      (FilePath.combine (Context.data_ context) "favicon.ico")
      Nothing
