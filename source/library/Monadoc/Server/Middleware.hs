module Monadoc.Server.Middleware where

import qualified GHC.Stack as Stack
import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Monadoc.Middleware.AddRequestId as AddRequestId
import qualified Monadoc.Middleware.CacheResponses as CacheResponses
import qualified Monadoc.Middleware.CompressResponses as CompressResponses
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Middleware.LogResponses as LogResponses
import qualified Monadoc.Middleware.TimeoutHandlers as TimeoutHandlers
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai as Wai

middleware :: (Stack.HasCallStack) => Context.Context -> Wai.Middleware
middleware context =
  AddRequestId.middleware (Context.key context)
    . LogResponses.middleware context
    . CacheResponses.middleware
    . CompressResponses.middleware (Context.temporaryDirectory context)
    . AddHeaders.middleware (Context.key context)
    . HandleExceptions.middleware context
    . TimeoutHandlers.middleware
