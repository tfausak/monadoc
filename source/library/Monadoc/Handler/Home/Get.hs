module Monadoc.Handler.Home.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Template.Home.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Model as Model
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http

handler :: Handler.Handler
handler _ respond = do
  context <- Reader.ask
  rows <-
    App.Sql.query_
      "select * \
      \ from upload \
      \ inner join package \
      \ on package.key = upload.package \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join hackageUser \
      \ on hackageUser.key = upload.uploadedBy \
      \ inner join packageMeta \
      \ on packageMeta.upload = upload.key \
      \ order by upload.uploadedAt desc \
      \ limit 64"
  let eTag = Common.makeETag $ case rows of
        (upload Sql.:. _) : _ -> Just upload.value.uploadedAt
        _ -> Nothing
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Nothing}
        ]
  respond
    . Common.htmlResponse
      Http.ok200
      [ (Http.hCacheControl, "max-age=3600, stale-while-revalidate=60"),
        (Http.hETag, eTag)
      ]
    $ Template.render
      context
      Template.Input
        { Template.breadcrumbs = breadcrumbs,
          Template.rows = rows
        }
