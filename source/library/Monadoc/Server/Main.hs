module Monadoc.Server.Main where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Class.MonadSay as MonadSay
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp

server :: Context.Context -> IO ()
server context =
  Warp.runSettings (getSettings $ Context.config context)
    . Middleware.middleware context
    $ Application.application context

getSettings :: Config.Config -> Warp.Settings
getSettings config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
    . Warp.setOnException (const HandleExceptions.onException)
    . Warp.setOnExceptionResponse HandleExceptions.onExceptionResponse
    . Warp.setPort (Config.port config)
    $ Warp.setServerName ByteString.empty Warp.defaultSettings

beforeMainLoop :: MonadSay.MonadSay m => Config.Config -> m ()
beforeMainLoop config = do
  MonadSay.sayString $
    "listening on "
      <> show (Config.host config)
      <> " port "
      <> show (Config.port config)
