import qualified Monadoc.Type.TaskSpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec spec

spec :: Hspec.Spec
spec = do
  Monadoc.Type.TaskSpec.spec
