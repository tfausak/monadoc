{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Handler.AppleTouchIcon.Get where

import qualified Control.Monad.Reader as Reader
import qualified Monadoc.Class.MonadFile as MonadFile
import qualified Monadoc.Constant.ContentType as ContentType
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler ::
  (MonadFile.MonadFile m, Reader.MonadReader Context.Context m) =>
  Wai.Request ->
  m Wai.Response
handler _ =
  Common.fileResponse
    Http.ok200
    [(Http.hContentType, ContentType.png)]
    "apple-touch-icon.png"
