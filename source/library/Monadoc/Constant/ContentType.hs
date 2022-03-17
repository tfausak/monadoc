{-# LANGUAGE TypeApplications #-}

module Monadoc.Constant.ContentType where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Vendor.Witch as Witch

css :: ByteString.ByteString
css = Witch.into @ByteString.ByteString "text/css;charset=utf-8"

html :: ByteString.ByteString
html = Witch.into @ByteString.ByteString "text/html;charset=utf-8"

ico :: ByteString.ByteString
ico = Witch.into @ByteString.ByteString "image/x-icon"

json :: ByteString.ByteString
json = Witch.into @ByteString.ByteString "application/json;charset=utf-8"

manifest :: ByteString.ByteString
manifest = Witch.into @ByteString.ByteString "application/manifest+json;charset=utf-8"

png :: ByteString.ByteString
png = Witch.into @ByteString.ByteString "image/png"

svg :: ByteString.ByteString
svg = Witch.into @ByteString.ByteString "image/svg+xml;charset=utf-8"

text :: ByteString.ByteString
text = Witch.into @ByteString.ByteString "text/plain;charset=utf-8"

xml :: ByteString.ByteString
xml = Witch.into @ByteString.ByteString "application/xml;charset=utf-8"
