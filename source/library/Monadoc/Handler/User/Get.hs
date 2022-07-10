{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.User.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Template.User.Get as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Witch

handler :: HackageUserName.HackageUserName -> Handler.Handler
handler hackageUserName _ respond = do
  context <- Reader.ask
  hackageUser <- do
    rows <- App.query "select * from hackageUser where name = ?" [hackageUserName]
    case rows of
      [] -> Traced.throw NotFound.NotFound
      row : _ -> pure row
  rows <-
    App.query
      "select * \
      \ from upload \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join package \
      \ on package.key = upload.package \
      \ where upload.uploadedBy = ? \
      \ order by upload.uploadedAt desc \
      \ limit 64"
      [Model.key hackageUser]
  let eTag = Common.makeETag $ case rows of
        (upload Sql.:. _) : _ -> Just . Upload.uploadedAt $ Model.value upload
        _ -> Nothing
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text hackageUserName, Breadcrumb.route = Nothing}
        ]
  packages <- App.query "select * from package where key in ( select distinct package from upload where uploadedBy = ? ) order by name collate nocase asc" [Model.key hackageUser]
  respond . Common.htmlResponse Http.ok200 [(Http.hETag, eTag)] $ Template.render context breadcrumbs hackageUser rows packages
