{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Server.Main where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp
import qualified Witch

server :: App.App ()
server = do
  context <- Reader.ask
  IO.liftIO
    . Warp.runSettings (getSettings context)
    . Middleware.middleware context
    $ Application.application context

getSettings :: Context.Context -> Warp.Settings
getSettings context =
  let config = Context.config context
   in Warp.setBeforeMainLoop (beforeMainLoop context)
        . Warp.setHost (Config.host config)
        . Warp.setOnException (HandleExceptions.onException context)
        . Warp.setOnExceptionResponse (HandleExceptions.onExceptionResponse context)
        . Warp.setPort (Witch.into @Int $ Config.port config)
        $ Warp.setServerName ByteString.empty Warp.defaultSettings

beforeMainLoop :: Context.Context -> IO ()
beforeMainLoop context = do
  let config = Context.config context
  App.runApp context . Log.info $
    Text.unwords
      [ "listening on",
        Text.pack . show $ Config.host config,
        "port",
        Text.pack . show $ Config.port config
      ]
