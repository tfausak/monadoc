module Monadoc.Middleware.CompressResponses where

import qualified Monadoc.Type.Context as Context
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as Gzip

middleware :: Context.Context -> Wai.Middleware
middleware context =
  Gzip.gzip
    Gzip.def
      { Gzip.gzipFiles = Gzip.GzipCacheFolder $ Context.temporaryDirectory context
      }
