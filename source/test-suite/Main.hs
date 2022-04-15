import qualified Monadoc.Spec as Monadoc
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec Monadoc.spec
