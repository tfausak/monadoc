{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Robots.Get where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.App as App
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Wai.Request -> App.App Wai.Response
handler _ =
  pure
    . Wai.responseLBS Http.ok200 [(Http.hContentType, ContentType.text)]
    . Witch.into @LazyByteString.ByteString
    $ unlines ["User-Agent: *", "Allow: /"]
