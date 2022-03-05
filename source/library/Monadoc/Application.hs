{-# LANGUAGE TypeApplications #-}

module Monadoc.Application where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paths_monadoc as Monadoc

application :: Wai.Application
application request respond =
  case fmap (Witch.into @String) $ Wai.pathInfo request of
    ["favicon.ico"] -> do
      filePath <- Monadoc.getDataFileName "logo.svg"
      respond $
        Wai.responseFile
          Http.ok200
          [(Http.hContentType, Witch.into @ByteString.ByteString "image/svg+xml")]
          filePath
          Nothing
    ["robots.txt"] -> do
      filePath <- Monadoc.getDataFileName "robots.txt"
      respond $
        Wai.responseFile
          Http.ok200
          [(Http.hContentType, Witch.into @ByteString.ByteString "text/plain;charset=utf-8")]
          filePath
          Nothing
    _ ->
      respond $
        Wai.responseLBS
          Http.notFound404
          [(Http.hContentType, Witch.into @ByteString.ByteString "text/plain;charset=utf-8")]
          (Witch.into @LazyByteString.ByteString "404 Not Found")
