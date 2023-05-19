module Monadoc.Middleware.TimeoutHandlers where

import qualified GHC.Stack as Stack
import qualified Monadoc.Exception.TimedOut as TimedOut
import qualified Monadoc.Exception.Traced as Traced
import qualified Network.Wai as Wai
import qualified System.Timeout as Timeout

middleware :: (Stack.HasCallStack) => Wai.Middleware
middleware handle request respond = do
  result <- Timeout.timeout 30_000_000 $ handle request respond
  maybe (Traced.throw TimedOut.TimedOut) pure result
