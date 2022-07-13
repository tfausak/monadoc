{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Robots.Get where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Type.Handler as Handler
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Witch

handler :: Handler.Handler
handler _ respond = do
  let body = Witch.into @LazyByteString.ByteString $ unlines ["User-Agent: *", "Allow: /"]
      eTag = Common.makeETag body
  respond $
    Wai.responseLBS
      Http.ok200
      [ (Http.hCacheControl, "max-age=604800, stale-while-revalidate=86400"),
        (Http.hContentType, ContentType.text),
        (Http.hETag, eTag)
      ]
      body
