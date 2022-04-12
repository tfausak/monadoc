module Monadoc.Handler.Common where

import qualified Lucid
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

htmlResponse :: Http.Status -> [Http.Header] -> Lucid.Html () -> Wai.Response
htmlResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.html) : headers)
    . Lucid.renderBS
