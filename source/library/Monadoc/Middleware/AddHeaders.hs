module Monadoc.Middleware.AddHeaders where

import qualified Data.ByteString as ByteString
import qualified Data.Function as Function
import qualified Data.Maybe as Maybe
import qualified Data.Vault.Lazy as Vault
import qualified Monadoc.Constant.Header as Header
import qualified Monadoc.Type.RequestId as RequestId
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware key handle request respond =
  handle request $
    respond
      . Wai.mapResponseHeaders
        ( addHeaders
            [ contentSecurityPolicy,
              (Header.contentTypeOptions, "nosniff"),
              (Header.frameOptions, "DENY"),
              ( Header.requestId,
                Witch.into @ByteString.ByteString
                  . Maybe.fromMaybe RequestId.zero
                  . Vault.lookup key
                  $ Wai.vault request
              ),
              (Header.referrerPolicy, "no-referrer"),
              strictTransportSecurity,
              (Header.xssProtection, "1; mode=block")
            ]
        )

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
