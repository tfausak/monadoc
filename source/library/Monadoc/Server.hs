module Monadoc.Server where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.IO as IO

server :: Context.Context -> IO ()
server context =
  Warp.runSettings (getSettings $ Context.config context) $ middleware application

getSettings :: Config.Config -> Warp.Settings
getSettings config =
  Warp.setBeforeMainLoop (putStrLn $ "listening on " <> show (Config.host config) <> " port " <> show (Config.port config))
    . Warp.setHost (Config.host config)
    . Warp.setOnException (const $ IO.hPrint IO.stderr . Exception.displayException)
    . Warp.setOnExceptionResponse (const $ Wai.responseLBS Http.internalServerError500 [] LazyByteString.empty)
    . Warp.setPort (Config.port config)
    $ Warp.setServerName ByteString.empty Warp.defaultSettings

middleware :: Wai.Middleware
middleware = id

application :: Wai.Application
application _ respond =
  respond $ Wai.responseLBS Http.ok200 [] LazyByteString.empty
