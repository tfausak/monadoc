{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Handler.Package.Get where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Template.Package.Get as Template
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai

handler :: (Reader.MonadReader Context.Context m, MonadSql.MonadSql m, Exception.MonadThrow m) => PackageName.PackageName -> Wai.Request -> m Wai.Response
handler packageName _ = do
  context <- Reader.ask
  package <- do
    rows <- MonadSql.query "select * from package where name = ?" [packageName]
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      row : _ -> pure row
  rows <-
    MonadSql.query
      "select * \
      \ from upload \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join hackageUser \
      \ on hackageUser.key = upload.uploadedBy \
      \ where upload.package = ? \
      \ order by upload.uploadedAt desc \
      \ limit 16"
      [Model.key package]
  let eTag = Common.makeETag $ case rows of
        (upload Sql.:. _) : _ -> Just . Upload.uploadedAt $ Model.value upload
        _ -> Nothing
  hackageUsers <- MonadSql.query "select * from hackageUser where key in (select distinct uploadedBy from upload where upload.package = ?) order by name collate nocase asc" [Model.key package]
  pure . Common.htmlResponse Http.ok200 [(Http.hETag, eTag)] $ Template.render context package rows hackageUsers
