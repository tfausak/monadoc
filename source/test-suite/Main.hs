import qualified Monadoc.Type.PortSpec
import qualified Monadoc.Type.StatusSpec
import qualified Monadoc.Type.TaskSpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec spec

spec :: Hspec.Spec
spec = do
  Monadoc.Type.PortSpec.spec
  Monadoc.Type.StatusSpec.spec
  Monadoc.Type.TaskSpec.spec
