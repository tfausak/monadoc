{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Class.MonadHttp where

import qualified Control.Monad.Trans.Control as Control
import qualified Network.HTTP.Client as Client

class Monad m => MonadHttp m where
  withManager :: (Client.Manager -> m a) -> m a

  withResponse :: Client.Request -> (Client.Response Client.BodyReader -> m a) -> m a
  default withResponse :: Control.MonadBaseControl IO m => Client.Request -> (Client.Response Client.BodyReader -> m a) -> m a
  withResponse request callback = withManager $ \manager ->
    Control.control $ \runInBase ->
      Client.withResponse request manager $ runInBase . callback

httpNoBody :: MonadHttp m => Client.Request -> m (Client.Response ())
httpNoBody request = withResponse request $ \response ->
  pure response {Client.responseBody = ()}
