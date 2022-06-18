{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Handler.Home.Get where

import qualified Control.Monad.Reader as Reader
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Template.Home.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
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
      \ limit 64"
  let eTag = Common.makeETag $ case rows of
        (upload Sql.:. _) : _ -> Just . Upload.uploadedAt $ Model.value upload
        _ -> Nothing
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Nothing}
        ]
      input =
        Template.Input
          { Template.breadcrumbs = breadcrumbs,
            Template.rows = rows
          }
  pure
    . Common.htmlResponse Http.ok200 [(Http.hETag, eTag)]
    $ Template.render context input
