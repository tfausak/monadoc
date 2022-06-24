{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Middleware.AddHeaders where

import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Monadoc.Constant.Header as Header
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

middleware :: String -> Wai.Middleware
middleware base =
  Wai.modifyResponse . Wai.mapResponseHeaders . addHeaders $
    Maybe.catMaybes
      [ Just contentSecurityPolicy,
        Just (Header.referrerPolicy, "no-referrer"),
        Just (Header.contentTypeOptions, "nosniff"),
        Just (Header.frameOptions, "DENY"),
        Just (Header.xssProtection, "1; mode=block"),
        if List.isPrefixOf "https:" base then Just strictTransportSecurity else Nothing
      ]

contentSecurityPolicy :: Http.Header
contentSecurityPolicy =
  ( Header.contentSecurityPolicy,
    "default-src 'none'; img-src 'self' data:; manifest-src 'self'; script-src 'self'; style-src 'self'"
  )

strictTransportSecurity :: Http.Header
strictTransportSecurity =
  ( Header.strictTransportSecurity,
    "max-age=31536000"
  )

addHeaders :: [Http.Header] -> Http.ResponseHeaders -> Http.ResponseHeaders
addHeaders = foldr addIfMissing

addIfMissing :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
addIfMissing = addIfMissingBy $ Function.on (==) fst

addIfMissingBy :: (a -> a -> Bool) -> a -> [a] -> [a]
addIfMissingBy p x xs = case xs of
  [] -> [x]
  h : t
    | p x h -> h : t
    | otherwise -> h : addIfMissingBy p x t
