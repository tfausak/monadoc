module Monadoc.Factory where

import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.Component.Upsert as Component.Upsert
import qualified Monadoc.Action.CronEntry.Upsert as CronEntry.Upsert
import qualified Monadoc.Action.Dependency.Upsert as Dependency.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Action.Module.Upsert as Module.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.PackageMeta.Upsert as PackageMeta.Upsert
import qualified Monadoc.Action.PackageMetaComponent.Upsert as PackageMetaComponent.Upsert
import qualified Monadoc.Action.PackageMetaComponentModule.Upsert as PackageMetaComponentModule.Upsert
import qualified Monadoc.Action.Preference.Upsert as Preference.Upsert
import qualified Monadoc.Action.Range.Upsert as Range.Upsert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Model.PackageMetaComponent as PackageMetaComponent
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

newBlob :: App.App Blob.Model
newBlob = do
  blob <- Test.arbitrary
  Blob.Upsert.run blob

newComponent :: App.App Component.Model
newComponent = do
  component <- Test.arbitrary
  Component.Upsert.run component

newCronEntry :: App.App CronEntry.Model
newCronEntry = do
  cronEntry <- Test.arbitrary
  CronEntry.Upsert.run cronEntry

newDependency :: App.App Dependency.Model
newDependency = do
  packageMetaComponent <- newPackageMetaComponent
  package <- newPackage
  component <- newComponent
  range <- newRange
  newDependencyWith (Model.key packageMetaComponent) (Model.key package) (Model.key component) (Model.key range)

newDependencyWith ::
  PackageMetaComponent.Key ->
  Package.Key ->
  Component.Key ->
  Range.Key ->
  App.App Dependency.Model
newDependencyWith packageMetaComponent package component range = do
  dependency <- Test.arbitrary
  Dependency.Upsert.run
    dependency
      { Dependency.packageMetaComponent = packageMetaComponent,
        Dependency.package = package,
        Dependency.component = component,
        Dependency.range = range
      }

newHackageUser :: App.App HackageUser.Model
newHackageUser = do
  hackageUser <- Test.arbitrary
  HackageUser.Upsert.run hackageUser

newLicense :: App.App License.Model
newLicense = do
  license <- Test.arbitrary
  License.Upsert.run license

newModule :: App.App Module.Model
newModule = do
  module_ <- Test.arbitrary
  Module.Upsert.run module_

newPackage :: App.App Package.Model
newPackage = do
  package <- Test.arbitrary
  Package.Upsert.run package

newPackageMeta :: App.App PackageMeta.Model
newPackageMeta = do
  version <- newVersion
  license <- newLicense
  upload <- newUpload
  newPackageMetaWith (Model.key version) (Model.key license) (Model.key upload)

newPackageMetaWith ::
  Version.Key ->
  License.Key ->
  Upload.Key ->
  App.App PackageMeta.Model
newPackageMetaWith version license upload = do
  packageMeta <- Test.arbitrary
  PackageMeta.Upsert.run
    packageMeta
      { PackageMeta.cabalVersion = version,
        PackageMeta.license = license,
        PackageMeta.upload = upload
      }

newPackageMetaComponent :: App.App PackageMetaComponent.Model
newPackageMetaComponent = do
  packageMeta <- newPackageMeta
  component <- newComponent
  newPackageMetaComponentWith (Model.key packageMeta) (Model.key component)

newPackageMetaComponentWith ::
  PackageMeta.Key ->
  Component.Key ->
  App.App PackageMetaComponent.Model
newPackageMetaComponentWith packageMeta component = do
  packageMetaComponent <- Test.arbitrary
  PackageMetaComponent.Upsert.run
    packageMetaComponent
      { PackageMetaComponent.packageMeta = packageMeta,
        PackageMetaComponent.component = component
      }

newPackageMetaComponentModule :: App.App PackageMetaComponentModule.Model
newPackageMetaComponentModule = do
  packageMetaComponent <- newPackageMetaComponent
  module_ <- newModule
  newPackageMetaComponentModuleWith (Model.key packageMetaComponent) (Model.key module_)

newPackageMetaComponentModuleWith ::
  PackageMetaComponent.Key ->
  Module.Key ->
  App.App PackageMetaComponentModule.Model
newPackageMetaComponentModuleWith packageMetaComponent module_ = do
  packageMetaComponentModule <- Test.arbitrary
  PackageMetaComponentModule.Upsert.run
    packageMetaComponentModule
      { PackageMetaComponentModule.packageMetaComponent = packageMetaComponent,
        PackageMetaComponentModule.module_ = module_
      }

newPreference :: App.App Preference.Model
newPreference = do
  package <- newPackage
  range <- newRange
  newPreferenceWith (Model.key package) (Model.key range)

newPreferenceWith ::
  Package.Key ->
  Range.Key ->
  App.App Preference.Model
newPreferenceWith package range = do
  preference <- Test.arbitrary
  Preference.Upsert.run
    preference
      { Preference.package = package,
        Preference.range = range
      }

newRange :: App.App Range.Model
newRange = do
  range <- Test.arbitrary
  Range.Upsert.run range

newUpload :: App.App Upload.Model
newUpload = do
  blob <- newBlob
  package <- newPackage
  hackageUser <- newHackageUser
  version <- newVersion
  newUploadWith (Model.key blob) (Model.key package) (Model.key hackageUser) (Model.key version)

newUploadWith ::
  Blob.Key ->
  Package.Key ->
  HackageUser.Key ->
  Version.Key ->
  App.App Upload.Model
newUploadWith blob package hackageUser version = do
  upload <- Test.arbitrary
  Upload.Upsert.run
    upload
      { Upload.blob = blob,
        Upload.package = package,
        Upload.uploadedBy = hackageUser,
        Upload.version = version
      }

newVersion :: App.App Version.Model
newVersion = do
  version <- Test.arbitrary
  Version.Upsert.run version
