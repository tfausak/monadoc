{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Common where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Hashable as Hashable
import qualified Lucid
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.Printf as Printf
import qualified Witch

htmlResponse :: Http.Status -> [Http.Header] -> Lucid.Html () -> Wai.Response
htmlResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.html) : headers)
    . Lucid.renderBS

makeETag :: Hashable.Hashable a => a -> ByteString.ByteString
makeETag =
  Witch.from @String @ByteString.ByteString
    . Printf.printf "\"%x\""
    . Hashable.hash

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.text) : headers)
    . Witch.into @LazyByteString.ByteString
    $ (Witch.into @ByteString.ByteString $ show (Http.statusCode status) <> " ")
      <> Http.statusMessage status
