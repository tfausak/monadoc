import qualified Control.Monad as Monad
import qualified Monadoc.Spec as Monadoc
import qualified System.Environment as Environment
import qualified Test.Hspec.Core.Formatters.V2 as Formatter
import qualified Test.Hspec.Core.Spec as Spec
import qualified Test.Hspec.Core.Util as Util
import qualified Test.Hspec.Runner as Runner
import qualified Text.Printf as Printf

main :: IO ()
main = do
  arguments <- Environment.getArgs
  config <- Runner.readConfig defaultConfig arguments
  summary <- Environment.withArgs [] $ Runner.runSpec Monadoc.spec config
  Runner.evaluateSummary summary

defaultConfig :: Runner.Config
defaultConfig =
  Runner.defaultConfig
    { Runner.configFormat =
        Just $
          Formatter.formatterToFormat
            Formatter.checks
              { Formatter.formatterDone = do
                  done
                  Formatter.formatterDone Formatter.checks
              }
    }

done :: Formatter.FormatM ()
done = do
  failures <- Formatter.getFailMessages
  Monad.when (not $ null failures) $ Formatter.writeLine ""
  Monad.forM_ failures $ \failure ->
    Formatter.writeLine $
      Printf.printf
        "FAILURE file %s line %d column %d path %s"
        (maybe "unknown" Spec.locationFile $ Formatter.failureRecordLocation failure)
        (maybe (-1) Spec.locationLine $ Formatter.failureRecordLocation failure)
        (maybe (-1) Spec.locationColumn $ Formatter.failureRecordLocation failure)
        (Util.formatRequirement $ Formatter.failureRecordPath failure)
