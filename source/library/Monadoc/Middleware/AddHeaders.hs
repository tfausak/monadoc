{-# LANGUAGE TypeApplications #-}

module Monadoc.Middleware.AddHeaders where

import qualified Data.ByteString as ByteString
import qualified Data.Function as Function
import qualified Monadoc.Constant.Header as Header
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

middleware :: Wai.Middleware
middleware =
  Wai.modifyResponse . Wai.mapResponseHeaders $
    addHeader Header.contentSecurityPolicy "default-src 'self'"
      . addHeader Header.referrerPolicy "no-referrer"
      . addHeader Header.contentTypeOptions "nosniff"
      . addHeader Header.frameOptions "DENY"
      . addHeader Header.xssProtection "1; mode=block"

addHeader :: Http.HeaderName -> String -> Http.ResponseHeaders -> Http.ResponseHeaders
addHeader k = addIfMissing . makeHeader k

makeHeader :: Http.HeaderName -> String -> Http.Header
makeHeader k = (,) k . Witch.into @ByteString.ByteString

addIfMissing :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
addIfMissing = addIfMissingBy $ Function.on (==) fst

addIfMissingBy :: (a -> a -> Bool) -> a -> [a] -> [a]
addIfMissingBy p x xs = case xs of
  [] -> [x]
  h : t
    | p x h -> h : t
    | otherwise -> h : addIfMissingBy p x t
