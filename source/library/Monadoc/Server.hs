module Monadoc.Server where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Application as Application
import qualified Monadoc.Middleware as Middleware
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp
import qualified Say as Say

server :: Context.Context -> IO ()
server context =
  Warp.runSettings (getSettings $ Context.config context) $
    Middleware.middleware Application.application

getSettings :: Config.Config -> Warp.Settings
getSettings config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
    . Warp.setOnException (const HandleExceptions.onException)
    . Warp.setOnExceptionResponse (const HandleExceptions.onExceptionResponse)
    . Warp.setPort (Config.port config)
    $ Warp.setServerName ByteString.empty Warp.defaultSettings

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = do
  Say.sayString $
    "listening on "
      <> show (Config.host config)
      <> " port "
      <> show (Config.port config)
