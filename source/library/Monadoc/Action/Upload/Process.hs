{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Action.Upload.Process where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.CabalSpecVersion as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Utils.ShortText as Cabal
import qualified Monadoc.Action.Component.Upsert as Component.Upsert
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Action.PackageMeta.Upsert as PackageMeta.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.InvalidGenericPackageDescription as InvalidGenericPackageDescription
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Witch

run ::
  ( Control.MonadBaseControl IO m,
    MonadSql.MonadSql m,
    Exception.MonadThrow m
  ) =>
  m ()
run =
  MonadSql.withConnection $ \connection ->
    Sql.streamLifted
      connection
      "select * from upload \
      \ inner join blob on blob.key = upload.blob \
      \ inner join package on package.key = upload.package \
      \ inner join version on version.key = upload.version"
      ()
      handleRow

handleRow ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  (Upload.Model Sql.:. Blob.Model Sql.:. Package.Model Sql.:. Version.Model) ->
  m ()
handleRow (upload Sql.:. blob Sql.:. package Sql.:. version) = do
  maybePackageMeta <- PackageMeta.selectByUpload $ Model.key upload
  let hash = hashBlob blob
  Monad.when (fmap hashPackageMeta maybePackageMeta /= Just hash) $ do
    let bs = Blob.contents $ Model.value blob
    gpd <- case Cabal.parseGenericPackageDescriptionMaybe bs of
      Nothing -> Exception.throwM $ InvalidGenericPackageDescription.InvalidGenericPackageDescription bs
      Just gpd -> pure gpd
    let pd = Cabal.packageDescription gpd
    checkPackageName package pd
    checkPackageVersion version pd
    cabalVersion <-
      Version.Upsert.run
        Version.Version
          { Version.number = Witch.from . Cabal.mkVersion . Cabal.cabalSpecToVersionDigits $ Cabal.specVersion pd
          }
    license <-
      License.Upsert.run
        License.License
          { License.spdx = Witch.from $ Cabal.license pd
          }
    -- TODO: Associate components with packages.
    mapM_ Component.Upsert.run $ getComponents gpd
    Monad.void $
      PackageMeta.Upsert.run
        PackageMeta.PackageMeta
          { PackageMeta.buildType = Witch.from $ Cabal.buildType pd,
            PackageMeta.cabalVersion = Model.key cabalVersion,
            PackageMeta.hash = hash,
            PackageMeta.license = Model.key license,
            PackageMeta.upload = Model.key upload,
            PackageMeta.author = shortTextToMaybeText $ Cabal.author pd,
            PackageMeta.bugReports = shortTextToMaybeText $ Cabal.bugReports pd,
            PackageMeta.category = shortTextToMaybeText $ Cabal.category pd,
            PackageMeta.copyright = shortTextToMaybeText $ Cabal.copyright pd,
            PackageMeta.description = shortTextToMaybeText $ Cabal.description pd,
            PackageMeta.homepage = shortTextToMaybeText $ Cabal.homepage pd,
            PackageMeta.maintainer = shortTextToMaybeText $ Cabal.maintainer pd,
            PackageMeta.pkgUrl = shortTextToMaybeText $ Cabal.pkgUrl pd,
            PackageMeta.stability = shortTextToMaybeText $ Cabal.stability pd,
            PackageMeta.synopsis = shortTextToMaybeText $ Cabal.synopsis pd
          }

getComponents :: Cabal.GenericPackageDescription -> [Component.Component]
getComponents gpd =
  mconcat
    [ case Cabal.condLibrary gpd of
        Nothing -> []
        Just _ -> [Component.Component ComponentType.Library . Witch.via @PackageName.PackageName . Cabal.pkgName . Cabal.package $ Cabal.packageDescription gpd],
      Component.Component ComponentType.Benchmark . Witch.from . fst <$> Cabal.condBenchmarks gpd,
      Component.Component ComponentType.Executable . Witch.from . fst <$> Cabal.condExecutables gpd,
      Component.Component ComponentType.ForeignLibrary . Witch.from . fst <$> Cabal.condForeignLibs gpd,
      Component.Component ComponentType.Library . Witch.from . fst <$> Cabal.condSubLibraries gpd,
      Component.Component ComponentType.TestSuite . Witch.from . fst <$> Cabal.condTestSuites gpd
    ]

checkPackageName :: Exception.MonadThrow m => Package.Model -> Cabal.PackageDescription -> m ()
checkPackageName p pd = do
  let expected = Witch.into @Cabal.PackageName . Package.name $ Model.value p
      actual = Cabal.pkgName $ Cabal.package pd
  Monad.when (actual /= expected) $
    Exception.throwM
      Mismatch.Mismatch
        { Mismatch.expected = expected,
          Mismatch.actual = actual
        }

checkPackageVersion :: Exception.MonadThrow m => Version.Model -> Cabal.PackageDescription -> m ()
checkPackageVersion v pd = do
  let expected = Witch.into @Cabal.Version . Version.number $ Model.value v
      actual = Cabal.pkgVersion $ Cabal.package pd
  Monad.when (actual /= expected) $
    Exception.throwM
      Mismatch.Mismatch
        { Mismatch.expected = expected,
          Mismatch.actual = actual
        }

salt :: ByteString.ByteString
salt = "2"

hashBlob :: Blob.Model -> Hash.Hash
hashBlob = Hash.new . mappend salt . Witch.from . Blob.hash . Model.value

hashPackageMeta :: PackageMeta.Model -> Hash.Hash
hashPackageMeta = PackageMeta.hash . Model.value

shortTextToMaybeText :: Cabal.ShortText -> Maybe Text.Text
shortTextToMaybeText shortText =
  let text = Text.pack $ Cabal.fromShortText shortText
   in if Text.all Char.isSpace text then Nothing else Just text
