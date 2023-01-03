module Monadoc.Extra.HttpClient where

import qualified Data.ByteString as ByteString
import qualified Data.Version as Version
import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Paths_monadoc as Monadoc
import qualified Witch
import qualified Witch.Encoding as Witch

ensureUserAgent :: Client.Request -> Client.Request
ensureUserAgent request =
  request
    { Client.requestHeaders = AddHeaders.addIfMissing (Http.hUserAgent, userAgent) $ Client.requestHeaders request
    }

userAgent :: ByteString.ByteString
userAgent =
  "Monadoc/"
    <> Witch.via @(Witch.UTF_8 ByteString.ByteString) (Version.showVersion Monadoc.version)
    <> " (https://github.com/tfausak/monadoc)"
