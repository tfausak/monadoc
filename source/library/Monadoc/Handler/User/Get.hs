{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Handler.User.Get where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Template.User.Get as Template
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Model as Model
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai

handler :: (Reader.MonadReader Context.Context m, MonadSql.MonadSql m, Exception.MonadThrow m) => HackageUserName.HackageUserName -> Wai.Request -> m Wai.Response
handler hackageUserName _ = do
  context <- Reader.ask
  hackageUser <- do
    rows <- MonadSql.query "select * from hackageUser where name = ?" [hackageUserName]
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      row : _ -> pure row
  rows <-
    MonadSql.query
      "select * \
      \ from upload \
      \ inner join version \
      \ on version.key = upload.version \
      \ inner join package \
      \ on package.key = upload.package \
      \ where upload.uploadedBy = ? \
      \ order by upload.uploadedAt desc \
      \ limit 16"
      [Model.key hackageUser]
  let eTag = Common.makeETag $ case rows of
        (upload Sql.:. _) : _ -> Just . Upload.uploadedAt $ Model.value upload
        _ -> Nothing
  pure . Common.htmlResponse Http.ok200 [(Http.hETag, eTag)] $ Template.render context hackageUser rows
