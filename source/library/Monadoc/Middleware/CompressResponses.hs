module Monadoc.Middleware.CompressResponses where

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as Gzip

middleware :: FilePath -> Wai.Middleware
middleware temporaryDirectory =
  Gzip.gzip
    Gzip.def
      { Gzip.gzipFiles = Gzip.GzipCacheFolder temporaryDirectory
      }
