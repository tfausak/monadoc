import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Typeable as Typeable
import qualified Monadoc.Spec as Monadoc
import qualified System.Environment as Environment
import qualified Test.Hspec.Core.Format as Format
import qualified Test.Hspec.Core.Formatters as Formatters
import qualified Test.Hspec.Runner as Runner

main :: IO ()
main = do
  arguments <- Environment.getArgs
  config <- Runner.readConfig Runner.defaultConfig arguments
  summary <- Environment.withArgs [] . Runner.runSpec Monadoc.spec $ appendCiFormat config
  Runner.evaluateSummary summary

appendCiFormat :: Runner.Config -> Runner.Config
appendCiFormat config = withFormat (getFormat config <> ciFormatter) config

withFormat :: (Format.FormatConfig -> IO Format.Format) -> Runner.Config -> Runner.Config
withFormat format config =
  config
    { Runner.configFormat = Just format,
      Runner.configFormatter = Nothing
    }

getFormat :: Runner.Config -> Format.FormatConfig -> IO Format.Format
getFormat = Maybe.fromMaybe defaultFormat . lookupFormat

lookupFormat :: Runner.Config -> Maybe (Format.FormatConfig -> IO Format.Format)
lookupFormat config = case Runner.configFormat config of
  Just f -> Just f
  Nothing -> case Runner.configFormatter config of
    Just x -> Just $ Formatters.formatterToFormat x
    Nothing -> Nothing

defaultFormat :: Format.FormatConfig -> IO Format.Format
defaultFormat = Formatters.formatterToFormat Formatters.checks

ciFormatter :: Applicative io => formatConfig -> io Format.Format
ciFormatter = const $ pure ciFormat

ciFormat :: Format.Format
ciFormat = whenDone $ \xs -> whenCi $ do
  putStrLn ""
  mapM_ (putStrLn . uncurry formatFailure) $
    Maybe.mapMaybe (toFailure . Format.itemResult . snd) xs

whenDone ::
  Applicative io =>
  ([(Runner.Path, Format.Item)] -> io ()) ->
  Format.Event ->
  io ()
whenDone callback event = case event of
  Format.Done xs -> callback xs
  _ -> pure ()

whenCi :: IO () -> IO ()
whenCi action = do
  ci <- Environment.lookupEnv "CI"
  Monad.when (ci == Just "true") action

toFailure :: Format.Result -> Maybe (Maybe Format.Location, Format.FailureReason)
toFailure result = case result of
  Format.Failure maybeLocation failureReason -> Just (maybeLocation, failureReason)
  _ -> Nothing

formatFailure :: Maybe Format.Location -> Format.FailureReason -> String
formatFailure maybeLocation failureReason =
  mconcat
    [ "FAILURE ",
      formatLocation maybeLocation,
      ": ",
      formatFailureReason failureReason
    ]

formatLocation :: Maybe Format.Location -> String
formatLocation maybeLocation =
  mconcat
    [ maybe "?" Format.locationFile maybeLocation,
      ":",
      maybe "0" (show . Format.locationLine) maybeLocation,
      ":",
      maybe "0" (show . Format.locationColumn) maybeLocation
    ]

formatFailureReason :: Format.FailureReason -> String
formatFailureReason failureReason = case failureReason of
  Format.NoReason -> "no reason"
  Format.Reason reason -> reason
  Format.ExpectedButGot prefix expected actual ->
    mconcat
      [ maybe "" (<> ": ") prefix,
        "expected ",
        expected,
        " but got ",
        actual
      ]
  Format.Error prefix (Exception.SomeException exception) ->
    mconcat
      [ maybe "" (<> ": ") prefix,
        show (Typeable.typeOf exception),
        ": ",
        Exception.displayException exception
      ]
