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
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.Wai.Handler.Warp as Warp

server :: (Base.MonadBase IO m, Reader.MonadReader Context.Context m) => m ()
server = do
  context <- Reader.ask
  Base.liftBase
    . Warp.runSettings (getSettings $ Context.config context)
    . Middleware.middleware context
    $ Application.application context

getSettings :: Config.Config -> Warp.Settings
getSettings config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
    . Warp.setOnException (const HandleExceptions.onException)
    . Warp.setOnExceptionResponse HandleExceptions.onExceptionResponse
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
