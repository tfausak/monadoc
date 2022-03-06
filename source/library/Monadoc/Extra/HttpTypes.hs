{-# LANGUAGE TypeApplications #-}

module Monadoc.Extra.HttpTypes where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http

hContentSecurityPolicy :: Http.HeaderName
hContentSecurityPolicy = Witch.via @ByteString.ByteString "Content-Security-Policy"

hLink :: Http.HeaderName
hLink = Witch.via @ByteString.ByteString "Link"

hReferrerPolicy :: Http.HeaderName
hReferrerPolicy = Witch.via @ByteString.ByteString "Referrer-Policy"

hXContentTypeOptions :: Http.HeaderName
hXContentTypeOptions = Witch.via @ByteString.ByteString "X-Content-Type-Options"

hXFrameOptions :: Http.HeaderName
hXFrameOptions = Witch.via @ByteString.ByteString "X-Frame-Options"

hXXssProtection :: Http.HeaderName
hXXssProtection = Witch.via @ByteString.ByteString "X-XSS-Protection"
