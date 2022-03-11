{-# LANGUAGE TypeApplications #-}

module Monadoc.Constant.Header where

import qualified Data.ByteString as ByteString
import Monadoc.Orphanage ()
import qualified Network.HTTP.Types as Http
import qualified Witch

contentSecurityPolicy :: Http.HeaderName
contentSecurityPolicy = Witch.via @ByteString.ByteString "Content-Security-Policy"

contentTypeOptions :: Http.HeaderName
contentTypeOptions = Witch.via @ByteString.ByteString "X-Content-Type-Options"

frameOptions :: Http.HeaderName
frameOptions = Witch.via @ByteString.ByteString "X-Frame-Options"

link :: Http.HeaderName
link = Witch.via @ByteString.ByteString "Link"

referrerPolicy :: Http.HeaderName
referrerPolicy = Witch.via @ByteString.ByteString "Referrer-Policy"

xssProtection :: Http.HeaderName
xssProtection = Witch.via @ByteString.ByteString "X-XSS-Protection"
