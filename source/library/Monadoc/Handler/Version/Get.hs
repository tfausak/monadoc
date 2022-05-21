{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Version.Get where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.Found as Found
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Template.Version.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Witch

handler ::
  (Reader.MonadReader Context.Context m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  PackageName.PackageName ->
  Reversion.Reversion ->
  Wai.Request ->
  m Wai.Response
handler packageName reversion _ = do
  context <- Reader.ask
  package <-
    selectFirst $
      MonadSql.query
        "select * from package where name = ?"
        [packageName]
  version <-
    selectFirst $
      MonadSql.query
        "select * from version where number = ?"
        [Reversion.version reversion]
  revision <- case Reversion.revision reversion of
    Nothing -> do
      upload <-
        selectFirst $
          MonadSql.query
            "select * from upload where package = ? and version = ? order by revision desc limit 1"
            (Model.key package, Model.key version)
      let route =
            Route.Version
              (Package.name $ Model.value package)
              Reversion.Reversion
                { Reversion.revision = Just . Upload.revision $ Model.value upload,
                  Reversion.version = Version.number $ Model.value version
                }
      Exception.throwM $ Found.Found route
    Just revision -> pure revision
  upload <-
    selectFirst $
      MonadSql.query
        "select * from upload where package = ? and version = ? and revision = ? limit 1"
        (Model.key package, Model.key version, revision)
  hackageUser <- selectFirst $ MonadSql.query "select * from hackageUser where key = ?" [Upload.uploadedBy $ Model.value upload]
  maybeLatest <-
    Maybe.listToMaybe
      <$> MonadSql.query
        "select * \
        \ from upload \
        \ inner join version \
        \ on version.key = upload.version \
        \ where upload.package = ? \
        \ and upload.isLatest = true \
        \ and upload.key != ? \
        \ limit 1"
        (Model.key package, Model.key upload)
  let eTag = Common.makeETag . Upload.uploadedAt $ Model.value upload
      breadcrumbs =
        [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text packageName, Breadcrumb.route = Just $ Route.Package packageName},
          Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text reversion, Breadcrumb.route = Nothing}
        ]
  pure . Common.htmlResponse Http.ok200 [(Http.hETag, eTag)] $ Template.render context breadcrumbs package version upload hackageUser maybeLatest

selectFirst :: Exception.MonadThrow m => m [a] -> m a
selectFirst query = do
  rows <- query
  case rows of
    [] -> Exception.throwM NotFound.NotFound
    row : _ -> pure row
