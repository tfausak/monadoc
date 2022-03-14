{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.LogResponses where

import qualified Data.Time as Time
import qualified GHC.Clock as Clock
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Numeric
import qualified Say
import qualified Witch

middleware :: Wai.Middleware
middleware handle request respond = do
  before <- Clock.getMonotonicTime
  handle request $ \response -> do
    after <- Clock.getMonotonicTime
    now <- Time.getCurrentTime
    Say.sayString $
      unwords
        [ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" now,
          show . Http.statusCode $ Wai.responseStatus response,
          Witch.unsafeInto @String $ Wai.requestMethod request,
          Witch.unsafeInto @String $ Wai.rawPathInfo request,
          Numeric.showFFloat (Just 3) (after - before) ""
        ]
    respond response
