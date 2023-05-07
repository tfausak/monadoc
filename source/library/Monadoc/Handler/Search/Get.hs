module Monadoc.Handler.Search.Get where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Text as Text
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Template.Search.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Handler as Handler
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Search as Search
import qualified Network.HTTP.Types as Http
import qualified Witch

handler :: Search.Search -> Handler.Handler
handler query _ respond = do
  context <- Reader.ask
  packages <-
    if Search.isBlank query
      then pure []
      else
        App.Sql.query
          "select * \
          \ from package \
          \ where name like ? escape '\\' \
          \ order by name collate nocase asc \
          \ limit 64"
          [like query]
  hackageUsers <-
    if Search.isBlank query
      then pure []
      else
        App.Sql.query
          "select * \
          \ from hackageUser \
          \ where name like ? escape '\\' \
          \ order by name collate nocase asc \
          \ limit 64"
          [like query]
  modules <-
    if Search.isBlank query
      then pure []
      else
        App.Sql.query
          "select package.*, version.*, upload.*, component.*, module.* \
          \ from module \
          \ inner join packageMetaComponentModule \
          \ on packageMetaComponentModule.module = module.key \
          \ inner join packageMetaComponent \
          \ on packageMetaComponent.key = packageMetaComponentModule.packageMetaComponent \
          \ inner join packageMeta \
          \ on packageMeta.key = packageMetaComponent.packageMeta \
          \ inner join component \
          \ on component.key = packageMetaComponent.component \
          \ inner join upload \
          \ on upload.key = packageMeta.upload \
          \ inner join package \
          \ on package.key = upload.package \
          \ inner join version \
          \ on version.key = upload.version \
          \ where module.name like ? escape '\\' \
          \ and upload.isLatest = true \
          \ order by module.name collate nocase asc \
          \ limit 64"
          [like query]
  let breadcrumbs =
        Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home}
          : if Search.isBlank query
            then [Breadcrumb.Breadcrumb {Breadcrumb.label = "Search", Breadcrumb.route = Nothing}]
            else
              [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Search", Breadcrumb.route = Just $ Route.Search Search.empty},
                Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text query, Breadcrumb.route = Nothing}
              ]
  respond
    . Common.htmlResponse
      Http.ok200
      [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600")
      ]
    $ Template.render context breadcrumbs query packages hackageUsers modules

like :: Search.Search -> Text.Text
like = Text.cons '%' . flip Text.snoc '%' . escape . Witch.into @Text.Text

escape :: Text.Text -> Text.Text
escape = Text.concatMap $ \c -> case c of
  '_' -> Text.pack ['\\', c]
  '\\' -> Text.pack ['\\', c]
  '%' -> Text.pack ['\\', c]
  _ -> Text.singleton c
