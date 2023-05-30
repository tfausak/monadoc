module Monadoc.Middleware.AddHeaders where

import qualified Data.ByteString as ByteString
import qualified Data.Function as Function
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified Monadoc.Constant.Header as Header
import qualified Monadoc.Type.RequestId as RequestId
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware key handle request respond =
  handle request $ respond . Wai.mapResponseHeaders (addHeaders $ headers key request)

headers :: Vault.Key RequestId.RequestId -> Wai.Request -> Http.ResponseHeaders
headers key request =
  [ (Header.contentSecurityPolicy, contentSecurityPolicy),
    (Header.contentTypeOptions, "nosniff"),
    (Header.frameOptions, "DENY"),
    (Header.requestId, requestId key request),
    (Header.referrerPolicy, "no-referrer"),
    (Header.strictTransportSecurity, strictTransportSecurity),
    (Header.xssProtection, "1; mode=block")
  ]

contentSecurityPolicy :: ByteString.ByteString
contentSecurityPolicy =
  Witch.via @(Witch.UTF_8 ByteString.ByteString) $
    Text.intercalate
      "; "
      [ "base-uri 'none'",
        "default-src 'none'",
        "form-action 'self'",
        "frame-ancestors 'none'",
        "img-src 'self' data:",
        "manifest-src 'self'",
        "script-src 'self'",
        "style-src 'self'"
      ]

requestId :: Vault.Key RequestId.RequestId -> Wai.Request -> ByteString.ByteString
requestId key =
  Witch.into @ByteString.ByteString
    . Maybe.fromMaybe RequestId.zero
    . Vault.lookup key
    . Wai.vault

strictTransportSecurity :: ByteString.ByteString
strictTransportSecurity = "max-age=31536000"

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
