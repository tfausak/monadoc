module Monadoc.Handler.Common where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Hashable as Hashable
import qualified Data.Text.Lazy as LazyText
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Witch

fileResponse ::
  Http.Status ->
  Http.ResponseHeaders ->
  FilePath ->
  App.App Wai.Response
fileResponse status headers file = do
  context <- Reader.ask
  let path = FilePath.combine (Config.data_ $ Context.config context) file
  modificationTime <- IO.liftIO $ Directory.getModificationTime path
  let eTag = makeETag $ Witch.into @Timestamp.Timestamp modificationTime
      cacheControl = "max-age=604800, stale-while-revalidate=86400" :: ByteString.ByteString
  pure $
    Wai.responseFile
      status
      ((Http.hCacheControl, cacheControl) : (Http.hETag, eTag) : headers)
      path
      Nothing

htmlResponse ::
  Http.Status ->
  Http.ResponseHeaders ->
  Html.Html () ->
  Wai.Response
htmlResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.html) : headers)
    . Html.renderBS

makeETag :: Hashable.Hashable a => a -> ByteString.ByteString
makeETag = Witch.via @(Witch.UTF_8 ByteString.ByteString) . F.format ("\"" F.% F.int F.% "\"") . Hashable.hashWithSalt 0

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.text) : headers)
    . Witch.via @(Witch.UTF_8 LazyByteString.ByteString)
    $ F.format
      (F.int F.% " " F.% F.text)
      (Http.statusCode status)
      (Witch.unsafeInto @LazyText.Text . Witch.into @(Witch.UTF_8 ByteString.ByteString) $ Http.statusMessage status)
