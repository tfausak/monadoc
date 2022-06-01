module Monadoc.Test.Spec where

import qualified Monadoc.Action.Blob.InsertSpec
import qualified Monadoc.Action.Blob.UpsertSpec
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
import qualified Monadoc.Action.Package.UpsertSpec
import qualified Monadoc.Action.Preference.UpsertSpec
import qualified Monadoc.Action.Range.InsertSpec
import qualified Monadoc.Action.Range.UpsertSpec
import qualified Monadoc.Action.Upload.UpsertSpec
import qualified Monadoc.Action.Version.UpsertSpec
import qualified Monadoc.Extra.DirectSqliteSpec
import qualified Monadoc.Extra.EitherSpec
import qualified Monadoc.Extra.ListSpec
import qualified Monadoc.Extra.TimeSpec
import qualified Monadoc.Model.BlobSpec
import qualified Monadoc.Model.CronEntrySpec
import qualified Monadoc.Model.HackageIndexSpec
import qualified Monadoc.Model.HackageUserSpec
import qualified Monadoc.Model.JobSpec
import qualified Monadoc.Model.MigrationSpec
import qualified Monadoc.Model.PackageSpec
import qualified Monadoc.Model.PreferenceSpec
import qualified Monadoc.Model.RangeSpec
import qualified Monadoc.Model.UploadSpec
import qualified Monadoc.Model.VersionSpec
import qualified Monadoc.Query.BlobSpec
import qualified Monadoc.Query.CronEntrySpec
import qualified Monadoc.Query.RangeSpec
import qualified Monadoc.Type.ConfigSpec
import qualified Monadoc.Type.ConstraintSpec
import qualified Monadoc.Type.FlagSpec
import qualified Monadoc.Type.GuidSpec
import qualified Monadoc.Type.HackageUserNameSpec
import qualified Monadoc.Type.HashSpec
import qualified Monadoc.Type.KeySpec
import qualified Monadoc.Type.ModelSpec
import qualified Monadoc.Type.PackageNameSpec
import qualified Monadoc.Type.PortSpec
import qualified Monadoc.Type.ReversionSpec
import qualified Monadoc.Type.RevisionSpec
import qualified Monadoc.Type.RouteSpec
import qualified Monadoc.Type.ScheduleSpec
import qualified Monadoc.Type.SeveritySpec
import qualified Monadoc.Type.StatusSpec
import qualified Monadoc.Type.TaskSpec
import qualified Monadoc.Type.TimestampSpec
import qualified Monadoc.Type.VersionNumberSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Monadoc.Action.Blob.InsertSpec.spec
  Monadoc.Action.Blob.UpsertSpec.spec
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
  Monadoc.Action.Package.UpsertSpec.spec
  Monadoc.Action.Preference.UpsertSpec.spec
  Monadoc.Action.Range.InsertSpec.spec
  Monadoc.Action.Range.UpsertSpec.spec
  Monadoc.Action.Upload.UpsertSpec.spec
  Monadoc.Action.Version.UpsertSpec.spec
  Monadoc.Extra.DirectSqliteSpec.spec
  Monadoc.Extra.EitherSpec.spec
  Monadoc.Extra.ListSpec.spec
  Monadoc.Extra.TimeSpec.spec
  Monadoc.Model.BlobSpec.spec
  Monadoc.Model.CronEntrySpec.spec
  Monadoc.Model.HackageIndexSpec.spec
  Monadoc.Model.HackageUserSpec.spec
  Monadoc.Model.JobSpec.spec
  Monadoc.Model.MigrationSpec.spec
  Monadoc.Model.PackageSpec.spec
  Monadoc.Model.PreferenceSpec.spec
  Monadoc.Model.RangeSpec.spec
  Monadoc.Model.UploadSpec.spec
  Monadoc.Model.VersionSpec.spec
  Monadoc.Query.BlobSpec.spec
  Monadoc.Query.CronEntrySpec.spec
  Monadoc.Query.RangeSpec.spec
  Monadoc.Type.ConfigSpec.spec
  Monadoc.Type.ConstraintSpec.spec
  Monadoc.Type.FlagSpec.spec
  Monadoc.Type.GuidSpec.spec
  Monadoc.Type.HackageUserNameSpec.spec
  Monadoc.Type.HashSpec.spec
  Monadoc.Type.KeySpec.spec
  Monadoc.Type.ModelSpec.spec
  Monadoc.Type.PackageNameSpec.spec
  Monadoc.Type.PortSpec.spec
  Monadoc.Type.ReversionSpec.spec
  Monadoc.Type.RevisionSpec.spec
  Monadoc.Type.RouteSpec.spec
  Monadoc.Type.ScheduleSpec.spec
  Monadoc.Type.SeveritySpec.spec
  Monadoc.Type.StatusSpec.spec
  Monadoc.Type.TaskSpec.spec
  Monadoc.Type.TimestampSpec.spec
  Monadoc.Type.VersionNumberSpec.spec
