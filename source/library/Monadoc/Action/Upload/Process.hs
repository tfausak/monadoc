{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Monadoc.Action.Upload.Process where

import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.CabalSpecVersion as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.Component as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Utils.ShortText as Cabal
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Component.Upsert as Component.Upsert
import qualified Monadoc.Action.ComponentModule.Upsert as ComponentModule.Upsert
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Action.Module.Upsert as Module.Upsert
import qualified Monadoc.Action.PackageMeta.Upsert as PackageMeta.Upsert
import qualified Monadoc.Action.PackageMetaComponent.Upsert as PackageMetaComponent.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Exception.InvalidGenericPackageDescription as InvalidGenericPackageDescription
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.ComponentModule as ComponentModule
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.ModuleType as ModuleType
import qualified Witch

run :: App.App ()
run =
  App.Sql.withConnection $ \connection ->
    Sql.streamLifted
      connection
      "select * from upload \
      \ inner join blob on blob.key = upload.blob \
      \ inner join package on package.key = upload.package \
      \ inner join version on version.key = upload.version"
      ()
      handleRow

handleRow ::
  (Upload.Model Sql.:. Blob.Model Sql.:. Package.Model Sql.:. Version.Model) ->
  App.App ()
handleRow (upload Sql.:. blob Sql.:. package Sql.:. version) = do
  maybePackageMeta <- PackageMeta.selectByUpload $ Model.key upload
  let hash = hashBlob blob
  Monad.when (fmap hashPackageMeta maybePackageMeta /= Just hash) $ do
    let bs = Blob.contents $ Model.value blob
    gpd <- case Cabal.parseGenericPackageDescriptionMaybe bs of
      Nothing -> Traced.throw $ InvalidGenericPackageDescription.InvalidGenericPackageDescription bs
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
    packageMeta <-
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

    Monad.forM_ (toComponents gpd) $ \(ucn, (c, _ds)) -> do
      -- TODO: Store dependencies.

      component <- Component.Upsert.run $ case c of
        Cabal.CBench _ -> Component.Component ComponentType.Benchmark $ Witch.from ucn
        Cabal.CExe _ -> Component.Component ComponentType.Executable $ Witch.from ucn
        Cabal.CFLib _ -> Component.Component ComponentType.ForeignLibrary $ Witch.from ucn
        Cabal.CLib _ -> Component.Component ComponentType.Library $ Witch.from ucn
        Cabal.CTest _ -> Component.Component ComponentType.TestSuite $ Witch.from ucn

      Monad.void . PackageMetaComponent.Upsert.run $
        PackageMetaComponent.PackageMetaComponent
          { PackageMetaComponent.packageMeta = Model.key packageMeta,
            PackageMetaComponent.component = Model.key component
          }

      Monad.forM_ (componentModules c) $ \module_ -> do
        model <- Module.Upsert.run module_
        Monad.void $
          ComponentModule.Upsert.run
            ComponentModule.ComponentModule
              { ComponentModule.component = Model.key component,
                ComponentModule.module_ = Model.key model
              }

toComponents ::
  Cabal.GenericPackageDescription ->
  [(Cabal.UnqualComponentName, (Cabal.Component, [Cabal.Dependency]))]
toComponents gpd =
  let ucn = Cabal.packageNameToUnqualComponentName . Cabal.pkgName . Cabal.package $ Cabal.packageDescription gpd
   in mconcat
        [ toComponent Cabal.CLib . (,) ucn <$> Maybe.maybeToList (Cabal.condLibrary gpd),
          toComponent Cabal.CBench <$> Cabal.condBenchmarks gpd,
          toComponent Cabal.CExe <$> Cabal.condExecutables gpd,
          toComponent Cabal.CFLib <$> Cabal.condForeignLibs gpd,
          toComponent Cabal.CLib <$> Cabal.condSubLibraries gpd,
          toComponent Cabal.CTest <$> Cabal.condTestSuites gpd
        ]

-- TODO: Somehow track conditionals.
toComponent ::
  (Semigroup a, Semigroup c) =>
  (a -> Cabal.Component) ->
  (Cabal.UnqualComponentName, Cabal.CondTree v c a) ->
  (Cabal.UnqualComponentName, (Cabal.Component, c))
toComponent f = fmap $ Bifunctor.first f . Cabal.ignoreConditions

componentModules :: Cabal.Component -> [Module.Module]
componentModules c = case c of
  Cabal.CBench x -> buildInfoModules $ Cabal.benchmarkBuildInfo x
  Cabal.CExe x -> buildInfoModules $ Cabal.buildInfo x
  Cabal.CFLib x -> buildInfoModules $ Cabal.foreignLibBuildInfo x
  Cabal.CLib x ->
    fmap (toModule ModuleType.Exposed) (Cabal.exposedModules x)
      <> buildInfoModules (Cabal.libBuildInfo x)
  Cabal.CTest x -> buildInfoModules $ Cabal.testBuildInfo x

buildInfoModules :: Cabal.BuildInfo -> [Module.Module]
buildInfoModules bi =
  mconcat
    [ toModule ModuleType.Autogen <$> Cabal.autogenModules bi,
      toModule ModuleType.Other <$> Cabal.otherModules bi,
      toModule ModuleType.Virtual <$> Cabal.virtualModules bi
    ]

-- TODO: Handle module types.
toModule :: ModuleType.ModuleType -> Cabal.ModuleName -> Module.Module
toModule _ n = Module.Module {Module.name = Witch.from n}

checkPackageName :: Package.Model -> Cabal.PackageDescription -> App.App ()
checkPackageName p pd = do
  let expected = Witch.into @Cabal.PackageName . Package.name $ Model.value p
      actual = Cabal.pkgName $ Cabal.package pd
  Monad.when (actual /= expected) $
    Traced.throw
      Mismatch.Mismatch
        { Mismatch.expected = expected,
          Mismatch.actual = actual
        }

checkPackageVersion :: Version.Model -> Cabal.PackageDescription -> App.App ()
checkPackageVersion v pd = do
  let expected = Witch.into @Cabal.Version . Version.number $ Model.value v
      actual = Cabal.pkgVersion $ Cabal.package pd
  Monad.when (actual /= expected) $
    Traced.throw
      Mismatch.Mismatch
        { Mismatch.expected = expected,
          Mismatch.actual = actual
        }

salt :: ByteString.ByteString
salt = "2022-07-23"

hashBlob :: Blob.Model -> Hash.Hash
hashBlob = Hash.new . mappend salt . Witch.from . Blob.hash . Model.value

hashPackageMeta :: PackageMeta.Model -> Hash.Hash
hashPackageMeta = PackageMeta.hash . Model.value

shortTextToMaybeText :: Cabal.ShortText -> Maybe Text.Text
shortTextToMaybeText shortText =
  let text = Text.pack $ Cabal.fromShortText shortText
   in if Text.all Char.isSpace text then Nothing else Just text
