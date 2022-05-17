{-# LANGUAGE FlexibleContexts #-}

module Monadoc.Handler.Search.Get where

import qualified Control.Monad.Reader as Reader
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Template.Search.Get as Template
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Query as Query
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Reader.MonadReader Context.Context m => Query.Query -> Wai.Request -> m Wai.Response
handler query _ = do
  context <- Reader.ask
  pure . Common.htmlResponse Http.ok200 [] $ Template.render context query
