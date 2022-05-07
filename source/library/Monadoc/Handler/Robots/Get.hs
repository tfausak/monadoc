{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Robots.Get where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Common as Common
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Witch

handler :: Applicative m => Wai.Request -> m Wai.Response
handler _ = do
  let body = Witch.into @LazyByteString.ByteString $ unlines ["User-Agent: *", "Allow: /"]
      eTag = Common.makeETag body
  pure $
    Wai.responseLBS Http.ok200 [(Http.hContentType, ContentType.text), (Http.hETag, eTag)] body
