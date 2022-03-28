module Monadoc.Class.MonadHttp where

import qualified Network.HTTP.Client as Client

class Monad m => MonadHttp m where
  withResponse :: Client.Request -> (Client.Response Client.BodyReader -> m a) -> m a

httpNoBody :: MonadHttp m => Client.Request -> m (Client.Response ())
httpNoBody request = withResponse request $ \response ->
  pure response {Client.responseBody = ()}
