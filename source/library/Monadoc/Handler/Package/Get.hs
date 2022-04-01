module Monadoc.Handler.Package.Get where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Type.PackageName as PackageName
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Applicative m => PackageName.PackageName -> Wai.Request -> m Wai.Response
handler _ _ = pure $ Wai.responseLBS Http.ok200 [] LazyByteString.empty
