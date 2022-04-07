import qualified Monadoc.Model.VersionSpec
import qualified Monadoc.Type.ConfigSpec
import qualified Monadoc.Type.ConstraintSpec
import qualified Monadoc.Type.FlagSpec
import qualified Monadoc.Type.HackageUserNameSpec
import qualified Monadoc.Type.HashSpec
import qualified Monadoc.Type.KeySpec
import qualified Monadoc.Type.ModelSpec
import qualified Monadoc.Type.PackageNameSpec
import qualified Monadoc.Type.PortSpec
import qualified Monadoc.Type.RevisionSpec
import qualified Monadoc.Type.RouteSpec
import qualified Monadoc.Type.SeveritySpec
import qualified Monadoc.Type.StatusSpec
import qualified Monadoc.Type.TaskSpec
import qualified Monadoc.Type.TimestampSpec
import qualified Monadoc.Type.VersionNumberSpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec spec

spec :: Hspec.Spec
spec = do
  Monadoc.Model.VersionSpec.spec
  Monadoc.Type.ConfigSpec.spec
  Monadoc.Type.ConstraintSpec.spec
  Monadoc.Type.FlagSpec.spec
  Monadoc.Type.HackageUserNameSpec.spec
  Monadoc.Type.HashSpec.spec
  Monadoc.Type.KeySpec.spec
  Monadoc.Type.ModelSpec.spec
  Monadoc.Type.PackageNameSpec.spec
  Monadoc.Type.PortSpec.spec
  Monadoc.Type.RevisionSpec.spec
  Monadoc.Type.RouteSpec.spec
  Monadoc.Type.SeveritySpec.spec
  Monadoc.Type.StatusSpec.spec
  Monadoc.Type.TaskSpec.spec
  Monadoc.Type.TimestampSpec.spec
  Monadoc.Type.VersionNumberSpec.spec
