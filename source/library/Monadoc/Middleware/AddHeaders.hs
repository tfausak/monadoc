module Monadoc.Middleware.AddHeaders where

import qualified Data.Function as Function
import qualified Monadoc.Constant.Header as Header
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

middleware :: Wai.Middleware
middleware =
  Wai.modifyResponse . Wai.mapResponseHeaders $
    addHeaders
      [ contentSecurityPolicy,
        (Header.referrerPolicy, "no-referrer"),
        (Header.contentTypeOptions, "nosniff"),
        (Header.frameOptions, "DENY"),
        (Header.xssProtection, "1; mode=block"),
        strictTransportSecurity
      ]

contentSecurityPolicy :: Http.Header
contentSecurityPolicy =
  ( Header.contentSecurityPolicy,
    "base-uri 'none'; \
    \default-src 'none'; \
    \form-action 'self'; \
    \img-src 'self' data:; \
    \manifest-src 'self'; \
    \script-src 'self'; \
    \style-src 'self'"
  )

strictTransportSecurity :: Http.Header
strictTransportSecurity =
  ( Header.strictTransportSecurity,
    "max-age=31536000"
  )

addHeaders :: [Http.Header] -> Http.ResponseHeaders -> Http.ResponseHeaders
addHeaders = foldr addIfMissing

addIfMissing :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
addIfMissing = addIfMissingBy $ Function.on (==) fst

addIfMissingBy :: (a -> a -> Bool) -> a -> [a] -> [a]
addIfMissingBy p x xs = case xs of
  [] -> [x]
  h : t
    | p x h -> h : t
    | otherwise -> h : addIfMissingBy p x t
