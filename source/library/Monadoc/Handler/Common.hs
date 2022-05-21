{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Common where

import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Hashable as Hashable
import qualified Lucid as Html
import qualified Monadoc.Class.MonadFile as MonadFile
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf
import qualified Witch

fileResponse ::
  (MonadFile.MonadFile m, Reader.MonadReader Context.Context m) =>
  Http.Status ->
  Http.ResponseHeaders ->
  FilePath ->
  m Wai.Response
fileResponse status headers file = do
  context <- Reader.ask
  let path = FilePath.combine (Context.data_ context) file
  modificationTime <- MonadFile.getModificationTime path
  let eTag = makeETag $ Witch.into @Timestamp.Timestamp modificationTime
  pure $ Wai.responseFile status ((Http.hETag, eTag) : headers) path Nothing

htmlResponse ::
  Http.Status ->
  Http.ResponseHeaders ->
  Html.Html () ->
  Wai.Response
htmlResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.html) : headers)
    . Html.renderBS

makeETag :: Hashable.Hashable a => a -> ByteString.ByteString
makeETag =
  Witch.from @String @ByteString.ByteString
    . Printf.printf "\"%x\""
    . Hashable.hashWithSalt 0

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS status ((Http.hContentType, ContentType.text) : headers)
    . Witch.into @LazyByteString.ByteString
    $ (Witch.into @ByteString.ByteString $ show (Http.statusCode status) <> " ")
      <> Http.statusMessage status
