{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Extra.HttpClient where

import qualified Data.ByteString as ByteString
import qualified Data.Version as Version
import qualified Monadoc.Middleware.AddHeaders as AddHeaders
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Paths_monadoc as Monadoc
import qualified Witch

ensureUserAgent :: Client.Request -> Client.Request
ensureUserAgent request =
  request
    { Client.requestHeaders = AddHeaders.addIfMissing (Http.hUserAgent, userAgent) $ Client.requestHeaders request
    }

userAgent :: ByteString.ByteString
userAgent =
  "Monadoc/"
    <> Witch.from (Version.showVersion Monadoc.version)
    <> " (https://github.com/tfausak/monadoc)"
