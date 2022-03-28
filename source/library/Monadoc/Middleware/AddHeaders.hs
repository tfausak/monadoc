{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Middleware.AddHeaders where

import qualified Data.Function as Function
import qualified Monadoc.Constant.Header as Header
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

middleware :: Wai.Middleware
middleware =
  Wai.modifyResponse . Wai.mapResponseHeaders $
    addHeaders
      [ (Header.contentSecurityPolicy, "default-src 'self'"),
        (Header.referrerPolicy, "no-referrer"),
        (Header.contentTypeOptions, "nosniff"),
        (Header.frameOptions, "DENY"),
        (Header.xssProtection, "1; mode=block")
      ]

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
