module Monadoc.Type.Route where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Witch

data Route
  = AppleTouchIcon
  | Favicon
  | HealthCheck
  | Home
  | Manifest
  | Package PackageName.PackageName
  | Robots
  | Script
  | Stylesheet
  | User HackageUserName.HackageUserName
  deriving (Eq, Show)

parse :: Exception.MonadThrow m => [Text.Text] -> m Route
parse texts = case texts of
  [] -> pure Home
  ["apple-touch-icon.png"] -> pure AppleTouchIcon
  ["favicon.ico"] -> pure Favicon
  ["health-check"] -> pure HealthCheck
  ["static", "monadoc.webmanifest"] -> pure Manifest
  ["package", p] -> Package <$> Either.throw (Witch.tryFrom p)
  ["robots.txt"] -> pure Robots
  ["static", "monadoc.js"] -> pure Script
  ["static", "monadoc.css"] -> pure Stylesheet
  ["user", u] -> User <$> Either.throw (Witch.tryFrom u)
  _ -> Exception.throwM $ UnknownRoute.UnknownRoute texts

render :: Route -> [Text.Text]
render route = case route of
  AppleTouchIcon -> ["apple-touch-icon.png"]
  Favicon -> ["favicon.ico"]
  HealthCheck -> ["health-check"]
  Home -> []
  Manifest -> ["static", "monadoc.webmanifest"]
  Package p -> ["package", Witch.from p]
  Robots -> ["robots.txt"]
  Script -> ["static", "monadoc.js"]
  Stylesheet -> ["static", "monadoc.css"]
  User u -> ["user", Witch.from u]
