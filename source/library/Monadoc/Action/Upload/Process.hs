{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Upload.Process where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Control as Control
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Query.Blob as Blob
import qualified Monadoc.Type.Model as Model
import qualified Witch

-- TODO: Skip processing uploads that haven't changed. Will need to hash them
-- with a salt that can be updated when changes to the code are made. Just
-- parsing all the uploads takes about a minute and a half.
run ::
  ( Control.MonadBaseControl IO m,
    MonadLog.MonadLog m,
    MonadSql.MonadSql m,
    Exception.MonadThrow m
  ) =>
  m ()
run = do
  MonadSql.withConnection $ \connection ->
    Sql.streamLifted connection "select * from upload" () $ \upload -> do
      maybeBlob <- Blob.selectByKey . Upload.blob $ Model.value upload
      case maybeBlob of
        Nothing -> MonadLog.warn "no blob found"
        Just blob ->
          case Cabal.parseGenericPackageDescriptionMaybe . Blob.contents $ Model.value blob of
            Nothing -> MonadLog.warn "failed to parse package description"
            Just gpd ->
              Monad.void $
                License.Upsert.run
                  License.License
                    { License.spdx = Witch.from . Cabal.license $ Cabal.packageDescription gpd
                    }
