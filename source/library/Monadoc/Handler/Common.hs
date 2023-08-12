module Monadoc.Handler.Common where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Hashable as Hashable
import qualified Data.Text.Lazy as LazyText
import qualified Formatting as F
import qualified Lucid as Html
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

htmlResponse ::
  Http.Status ->
  Http.ResponseHeaders ->
  Html.Html () ->
  Wai.Response
htmlResponse status headers =
  Wai.responseLBS status (AddHeaders.addHeaders [(Http.hContentType, ContentType.html)] headers)
    . Html.renderBS

makeETag :: (Hashable.Hashable a) => a -> ByteString.ByteString
makeETag =
  Witch.via @(Witch.UTF_8 ByteString.ByteString)
    . F.format ("\"" F.% F.int F.% "\"")
    . Hashable.hashWithSalt 0

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS status (AddHeaders.addHeaders [(Http.hContentType, ContentType.text)] headers)
    . Witch.via @(Witch.UTF_8 LazyByteString.ByteString)
    $ F.format
      (F.int F.%+ F.text)
      (Http.statusCode status)
      (Witch.unsafeInto @LazyText.Text . Witch.into @(Witch.UTF_8 ByteString.ByteString) $ Http.statusMessage status)
