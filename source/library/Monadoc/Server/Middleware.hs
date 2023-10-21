module Monadoc.Server.Middleware where

import qualified GHC.Stack as Stack
import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Monadoc.Middleware.AddRequestId as AddRequestId
import qualified Monadoc.Middleware.CacheResponses as CacheResponses
import qualified Monadoc.Middleware.CompressResponses as CompressResponses
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Middleware.LogResponses as LogResponses
import qualified Monadoc.Middleware.ServeStaticFiles as ServeStaticFiles
import qualified Monadoc.Middleware.TimeoutHandlers as TimeoutHandlers
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai as Wai

middleware :: (Stack.HasCallStack) => Context.Context -> Wai.Middleware
middleware context =
  AddRequestId.middleware context.key
    . LogResponses.middleware context
    . CacheResponses.middleware
    . CompressResponses.middleware context.temporaryDirectory
    . AddHeaders.middleware context.key
    . HandleExceptions.middleware context
    . TimeoutHandlers.middleware
    . ServeStaticFiles.middleware
      context.cacheContainer
      context.config.data_
