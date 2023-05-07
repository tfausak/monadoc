module Monadoc.Handler.Package.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Template.Package.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Witch

handler :: PackageName.PackageName -> Handler.Handler
handler packageName _ respond = do
  context <- Reader.ask
  package <- do
    packages <- App.Sql.query "select * from package where name = ? limit 1" [packageName]
    NotFound.fromList packages
  rows <-
    App.Sql.query
      "select * \
      \ from upload \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join hackageUser \
      \ on hackageUser.key = upload.uploadedBy \
      \ where upload.package = ? \
      \ order by upload.uploadedAt desc"
      [Model.key package]
  let eTag = Common.makeETag $ case rows of
        (upload Sql.:. _) : _ -> Just . Upload.uploadedAt $ Model.value upload
        _ -> Nothing
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text packageName, Breadcrumb.route = Nothing}
        ]
  hackageUsers <- App.Sql.query "select * from hackageUser where key in (select distinct uploadedBy from upload where upload.package = ?) order by name collate nocase asc" [Model.key package]
  respond
    . Common.htmlResponse
      Http.ok200
      [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600"),
        (Http.hETag, eTag)
      ]
    $ Template.render context breadcrumbs package rows hackageUsers
