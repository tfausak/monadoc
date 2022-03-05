module Monadoc.Middleware where

import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Monadoc.Middleware.LogResponses as LogResponses
import qualified Network.Wai as Wai

middleware :: Wai.Middleware
middleware =
  LogResponses.middleware
    . AddHeaders.middleware
