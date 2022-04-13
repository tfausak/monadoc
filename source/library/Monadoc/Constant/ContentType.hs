module Monadoc.Constant.ContentType where

import qualified Data.ByteString as ByteString

css :: ByteString.ByteString
css = "text/css;charset=utf-8"

html :: ByteString.ByteString
html = "text/html;charset=utf-8"

ico :: ByteString.ByteString
ico = "image/x-icon"

js :: ByteString.ByteString
js = "application/javascript;charset=utf-8"

json :: ByteString.ByteString
json = "application/json;charset=utf-8"

manifest :: ByteString.ByteString
manifest = "application/manifest+json;charset=utf-8"

png :: ByteString.ByteString
png = "image/png"

svg :: ByteString.ByteString
svg = "image/svg+xml;charset=utf-8"

text :: ByteString.ByteString
text = "text/plain;charset=utf-8"

xml :: ByteString.ByteString
xml = "application/xml;charset=utf-8"
