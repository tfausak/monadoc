module Monadoc.Middleware.CacheResponses where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Handler.Common as Common
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai

middleware :: Wai.Middleware
middleware handle request respond = do
  handle request $ \response -> do
    let isGet = Wai.requestMethod request == Http.methodGet
        isSuccessful = Http.statusIsSuccessful $ Wai.responseStatus response
        expected = lookup Http.hETag $ Wai.responseHeaders response
        hasETag = Maybe.isJust expected
        actual = lookup Http.hIfNoneMatch $ Wai.requestHeaders request
        eTagsMatch = actual == expected
    respond $
      if isGet && isSuccessful && hasETag && eTagsMatch
        then Common.statusResponse Http.notModified304 $ Wai.responseHeaders response
        else response
