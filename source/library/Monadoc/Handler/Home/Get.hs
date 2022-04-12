module Monadoc.Handler.Home.Get where

import qualified Control.Monad.Reader as Reader
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Template.Home.Get as Template
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: (Reader.MonadReader Context.Context m, MonadSql.MonadSql m) => Wai.Request -> m Wai.Response
handler _ = do
  context <- Reader.ask
  rows <-
    MonadSql.query_
      "select * \
      \ from upload \
      \ inner join package \
      \ on package.key = upload.package \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join hackageUser \
      \ on hackageUser.key = upload.uploadedBy \
      \ order by upload.uploadedAt desc \
      \ limit 16"
  pure . Common.htmlResponse Http.ok200 [] $ Template.render context rows
