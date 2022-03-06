module Monadoc.Middleware where

import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Middleware.LogResponses as LogResponses
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as Gzip

middleware :: Wai.Middleware
middleware =
  LogResponses.middleware
    . Gzip.gzip Gzip.def
    . AddHeaders.middleware
    . HandleExceptions.middleware
