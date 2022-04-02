module Monadoc.Handler.Robots.Get where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

handler :: Applicative m => Wai.Request -> m Wai.Response
handler _ =
  pure
    . Wai.responseLBS Http.ok200 [(Http.hContentType, ContentType.text)]
    . Witch.into @LazyByteString.ByteString
    $ unlines ["User-Agent: *", "Allow: /"]
