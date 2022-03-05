{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.AddHeaders where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

middleware :: Wai.Middleware
middleware =
  Wai.modifyResponse . Wai.mapResponseHeaders $
    addHeader "Content-Security-Policy" "default-src 'self'"
      . addHeader "Referrer-Policy" "no-referrer"
      . addHeader "X-Content-Type-Options" "nosniff"
      . addHeader "X-Frame-Options" "DENY"
      . addHeader "X-XSS-Protection" "1; mode=block"

addHeader :: String -> String -> Http.ResponseHeaders -> Http.ResponseHeaders
addHeader k = addIfMissing . makeHeader k

makeHeader :: String -> String -> Http.Header
makeHeader k v = (Witch.via @ByteString.ByteString k, Witch.into @ByteString.ByteString v)

addIfMissing :: Http.Header -> Http.ResponseHeaders -> Http.ResponseHeaders
addIfMissing header responseHeaders = case responseHeaders of
  [] -> [header]
  x : ys ->
    if fst x == fst header
      then x : ys
      else x : addIfMissing header ys
