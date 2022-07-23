module MonadocSpec where

import qualified Monadoc.Action.Blob.InsertSpec
import qualified Monadoc.Action.Blob.UpsertSpec
import qualified Monadoc.Action.Component.InsertSpec
import qualified Monadoc.Action.Component.UpsertSpec
import qualified Monadoc.Action.ComponentModule.InsertSpec
import qualified Monadoc.Action.ComponentModule.UpsertSpec
import qualified Monadoc.Action.CronEntry.DeleteSpec
import qualified Monadoc.Action.CronEntry.EnqueueSpec
import qualified Monadoc.Action.CronEntry.InsertSpec
import qualified Monadoc.Action.CronEntry.PruneSpec
import qualified Monadoc.Action.CronEntry.UpdateSpec
import qualified Monadoc.Action.CronEntry.UpsertSpec
import qualified Monadoc.Action.Database.InitializeSpec
import qualified Monadoc.Action.Database.VacuumSpec
import qualified Monadoc.Action.HackageUser.UpsertSpec
import qualified Monadoc.Action.Job.EnqueueSpec
import qualified Monadoc.Action.Key.SelectLastInsertSpec
import qualified Monadoc.Action.License.InsertSpec
import qualified Monadoc.Action.License.UpsertSpec
import qualified Monadoc.Action.Module.InsertSpec
import qualified Monadoc.Action.Module.UpsertSpec
import qualified Monadoc.Action.Package.UpsertSpec
import qualified Monadoc.Action.PackageMeta.InsertSpec
import qualified Monadoc.Action.PackageMeta.UpdateSpec
import qualified Monadoc.Action.PackageMeta.UpsertSpec
import qualified Monadoc.Action.PackageMetaComponent.InsertSpec
import qualified Monadoc.Action.PackageMetaComponent.UpsertSpec
import qualified Monadoc.Action.Preference.UpsertSpec
import qualified Monadoc.Action.Range.InsertSpec
import qualified Monadoc.Action.Range.UpsertSpec
import qualified Monadoc.Action.Upload.UpsertSpec
import qualified Monadoc.Action.Version.UpsertSpec
import qualified Monadoc.Extra.DirectSqliteSpec
import qualified Monadoc.Extra.EitherSpec
import qualified Monadoc.Extra.HttpClientSpec
import qualified Monadoc.Extra.ListSpec
import qualified Monadoc.Extra.MaybeSpec
import qualified Monadoc.Extra.TimeSpec
import qualified Monadoc.Model.BlobSpec
import qualified Monadoc.Model.ComponentModuleSpec
import qualified Monadoc.Model.ComponentSpec
import qualified Monadoc.Model.CronEntrySpec
import qualified Monadoc.Model.HackageIndexSpec
import qualified Monadoc.Model.HackageUserSpec
import qualified Monadoc.Model.JobSpec
import qualified Monadoc.Model.LicenseSpec
import qualified Monadoc.Model.MigrationSpec
import qualified Monadoc.Model.ModuleSpec
import qualified Monadoc.Model.PackageMetaComponentSpec
import qualified Monadoc.Model.PackageMetaSpec
import qualified Monadoc.Model.PackageSpec
import qualified Monadoc.Model.PreferenceSpec
import qualified Monadoc.Model.RangeSpec
import qualified Monadoc.Model.UploadSpec
import qualified Monadoc.Model.VersionSpec
import qualified Monadoc.Query.BlobSpec
import qualified Monadoc.Query.ComponentModuleSpec
import qualified Monadoc.Query.ComponentSpec
import qualified Monadoc.Query.CronEntrySpec
import qualified Monadoc.Query.LicenseSpec
import qualified Monadoc.Query.ModuleSpec
import qualified Monadoc.Query.PackageMetaComponentSpec
import qualified Monadoc.Query.PackageMetaSpec
import qualified Monadoc.Query.RangeSpec
import qualified Monadoc.Type.BuildTypeSpec
import qualified Monadoc.Type.ComponentNameSpec
import qualified Monadoc.Type.ComponentTypeSpec
import qualified Monadoc.Type.ConfigSpec
import qualified Monadoc.Type.ConstraintSpec
import qualified Monadoc.Type.FlagSpec
import qualified Monadoc.Type.GuidSpec
import qualified Monadoc.Type.HackageUserNameSpec
import qualified Monadoc.Type.HashSpec
import qualified Monadoc.Type.KeySpec
import qualified Monadoc.Type.ModelSpec
import qualified Monadoc.Type.ModuleNameSpec
import qualified Monadoc.Type.ModuleTypeSpec
import qualified Monadoc.Type.PackageNameSpec
import qualified Monadoc.Type.PortSpec
import qualified Monadoc.Type.ReversionSpec
import qualified Monadoc.Type.RevisionSpec
import qualified Monadoc.Type.RouteSpec
import qualified Monadoc.Type.ScheduleSpec
import qualified Monadoc.Type.SeveritySpec
import qualified Monadoc.Type.SpdxSpec
import qualified Monadoc.Type.StatusSpec
import qualified Monadoc.Type.TaskSpec
import qualified Monadoc.Type.TimestampSpec
import qualified Monadoc.Type.UrlSpec
import qualified Monadoc.Type.VersionNumberSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Monadoc.Action.Blob.InsertSpec.spec
  Monadoc.Action.Blob.UpsertSpec.spec
  Monadoc.Action.Component.InsertSpec.spec
  Monadoc.Action.Component.UpsertSpec.spec
  Monadoc.Action.ComponentModule.InsertSpec.spec
  Monadoc.Action.ComponentModule.UpsertSpec.spec
  Monadoc.Action.CronEntry.DeleteSpec.spec
  Monadoc.Action.CronEntry.EnqueueSpec.spec
  Monadoc.Action.CronEntry.InsertSpec.spec
  Monadoc.Action.CronEntry.PruneSpec.spec
  Monadoc.Action.CronEntry.UpdateSpec.spec
  Monadoc.Action.CronEntry.UpsertSpec.spec
  Monadoc.Action.Database.InitializeSpec.spec
  Monadoc.Action.Database.VacuumSpec.spec
  Monadoc.Action.HackageUser.UpsertSpec.spec
  Monadoc.Action.Job.EnqueueSpec.spec
  Monadoc.Action.Key.SelectLastInsertSpec.spec
  Monadoc.Action.License.InsertSpec.spec
  Monadoc.Action.License.UpsertSpec.spec
  Monadoc.Action.Module.InsertSpec.spec
  Monadoc.Action.Module.UpsertSpec.spec
  Monadoc.Action.Package.UpsertSpec.spec
  Monadoc.Action.PackageMeta.InsertSpec.spec
  Monadoc.Action.PackageMeta.UpdateSpec.spec
  Monadoc.Action.PackageMeta.UpsertSpec.spec
  Monadoc.Action.PackageMetaComponent.InsertSpec.spec
  Monadoc.Action.PackageMetaComponent.UpsertSpec.spec
  Monadoc.Action.Preference.UpsertSpec.spec
  Monadoc.Action.Range.InsertSpec.spec
  Monadoc.Action.Range.UpsertSpec.spec
  Monadoc.Action.Upload.UpsertSpec.spec
  Monadoc.Action.Version.UpsertSpec.spec
  Monadoc.Extra.DirectSqliteSpec.spec
  Monadoc.Extra.EitherSpec.spec
  Monadoc.Extra.HttpClientSpec.spec
  Monadoc.Extra.ListSpec.spec
  Monadoc.Extra.MaybeSpec.spec
  Monadoc.Extra.TimeSpec.spec
  Monadoc.Model.BlobSpec.spec
  Monadoc.Model.ComponentModuleSpec.spec
  Monadoc.Model.ComponentSpec.spec
  Monadoc.Model.CronEntrySpec.spec
  Monadoc.Model.HackageIndexSpec.spec
  Monadoc.Model.HackageUserSpec.spec
  Monadoc.Model.JobSpec.spec
  Monadoc.Model.LicenseSpec.spec
  Monadoc.Model.MigrationSpec.spec
  Monadoc.Model.ModuleSpec.spec
  Monadoc.Model.PackageMetaComponentSpec.spec
  Monadoc.Model.PackageMetaSpec.spec
  Monadoc.Model.PackageSpec.spec
  Monadoc.Model.PreferenceSpec.spec
  Monadoc.Model.RangeSpec.spec
  Monadoc.Model.UploadSpec.spec
  Monadoc.Model.VersionSpec.spec
  Monadoc.Query.BlobSpec.spec
  Monadoc.Query.ComponentModuleSpec.spec
  Monadoc.Query.ComponentSpec.spec
  Monadoc.Query.CronEntrySpec.spec
  Monadoc.Query.LicenseSpec.spec
  Monadoc.Query.ModuleSpec.spec
  Monadoc.Query.PackageMetaComponentSpec.spec
  Monadoc.Query.PackageMetaSpec.spec
  Monadoc.Query.RangeSpec.spec
  Monadoc.Type.BuildTypeSpec.spec
  Monadoc.Type.ComponentNameSpec.spec
  Monadoc.Type.ComponentTypeSpec.spec
  Monadoc.Type.ConfigSpec.spec
  Monadoc.Type.ConstraintSpec.spec
  Monadoc.Type.FlagSpec.spec
  Monadoc.Type.GuidSpec.spec
  Monadoc.Type.HackageUserNameSpec.spec
  Monadoc.Type.HashSpec.spec
  Monadoc.Type.KeySpec.spec
  Monadoc.Type.ModelSpec.spec
  Monadoc.Type.ModuleNameSpec.spec
  Monadoc.Type.ModuleTypeSpec.spec
  Monadoc.Type.PackageNameSpec.spec
  Monadoc.Type.PortSpec.spec
  Monadoc.Type.ReversionSpec.spec
  Monadoc.Type.RevisionSpec.spec
  Monadoc.Type.RouteSpec.spec
  Monadoc.Type.ScheduleSpec.spec
  Monadoc.Type.SeveritySpec.spec
  Monadoc.Type.SpdxSpec.spec
  Monadoc.Type.StatusSpec.spec
  Monadoc.Type.TaskSpec.spec
  Monadoc.Type.TimestampSpec.spec
  Monadoc.Type.UrlSpec.spec
  Monadoc.Type.VersionNumberSpec.spec
