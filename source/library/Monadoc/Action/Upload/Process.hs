{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Upload.Process where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Distribution.CabalSpecVersion as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Action.PackageMeta.Insert as PackageMeta.Insert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.Blob as Blob
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Witch

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
        Nothing -> MonadLog.warn $ "no blob found for upload " <> Witch.from (Model.key upload)
        Just blob -> do
          maybePackageMeta <- PackageMeta.selectByUpload $ Model.key upload
          let hash = expected blob
          case maybePackageMeta of
            Just packageMeta | actual packageMeta == hash -> pure ()
            _ -> case Cabal.parseGenericPackageDescriptionMaybe . Blob.contents $ Model.value blob of
              Nothing -> MonadLog.warn $ "failed to parse package description for upload " <> Witch.from (Model.key upload)
              Just gpd -> do
                let pd = Cabal.packageDescription gpd
                version <- Version.Upsert.run Version.Version {Version.number = Witch.from . Cabal.mkVersion . Cabal.cabalSpecToVersionDigits $ Cabal.specVersion pd}
                license <- License.Upsert.run License.License {License.spdx = Witch.from $ Cabal.license pd}
                Monad.void $
                  PackageMeta.Insert.run
                    PackageMeta.PackageMeta
                      { PackageMeta.buildType = Witch.from $ Cabal.buildType pd,
                        PackageMeta.cabalVersion = Model.key version,
                        PackageMeta.hash = hash,
                        PackageMeta.license = Model.key license,
                        PackageMeta.upload = Model.key upload
                      }

salt :: ByteString.ByteString
salt = ""

expected :: Blob.Model -> Hash.Hash
expected = Hash.new . mappend salt . Witch.from . Blob.hash . Model.value

actual :: PackageMeta.Model -> Hash.Hash
actual = PackageMeta.hash . Model.value
