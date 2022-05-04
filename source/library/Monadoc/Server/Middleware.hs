module Monadoc.Server.Middleware where

import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Monadoc.Middleware.CacheResponses as CacheResponses
import qualified Monadoc.Middleware.CompressResponses as CompressResponses
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Middleware.LogResponses as LogResponses
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai as Wai

middleware :: Context.Context -> Wai.Middleware
middleware context =
  LogResponses.middleware
    . CacheResponses.middleware
    . CompressResponses.middleware (Context.temporaryDirectory context)
    . AddHeaders.middleware (Config.base $ Context.config context)
    . HandleExceptions.middleware
