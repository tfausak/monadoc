{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Server.Main where

import qualified Control.Monad.Base as Base
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp
import qualified Witch

server :: (Base.MonadBase IO m, Reader.MonadReader Context.Context m) => m ()
server = do
  context <- Reader.ask
  Base.liftBase
    . Warp.runSettings (getSettings context)
    . Middleware.middleware context
    $ Application.application context

getSettings :: Context.Context -> Warp.Settings
getSettings context =
  let config = Context.config context
   in Warp.setBeforeMainLoop (beforeMainLoop config)
        . Warp.setHost (Config.host config)
        . Warp.setOnException (const HandleExceptions.onException)
        . Warp.setOnExceptionResponse (HandleExceptions.onExceptionResponse context)
        . Warp.setPort (Witch.into @Int $ Config.port config)
        $ Warp.setServerName ByteString.empty Warp.defaultSettings

beforeMainLoop :: MonadLog.MonadLog m => Config.Config -> m ()
beforeMainLoop config = do
  MonadLog.info $
    Text.unwords
      [ "listening on",
        Text.pack . show $ Config.host config,
        "port",
        Text.pack . show $ Config.port config
      ]
