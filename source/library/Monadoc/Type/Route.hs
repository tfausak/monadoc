module Monadoc.Type.Route where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Type.PackageName as PackageName
import qualified Witch

data Route
  = AppleTouchIcon
  | Bootstrap
  | Favicon
  | HealthCheck
  | Home
  | Manifest
  | Package PackageName.PackageName
  | Robots
  deriving (Eq, Show)

parse :: Exception.MonadThrow m => [Text.Text] -> m Route
parse texts = case texts of
  [] -> pure Home
  ["apple-touch-icon.png"] -> pure AppleTouchIcon
  ["favicon.ico"] -> pure Favicon
  ["health-check"] -> pure HealthCheck
  ["monadoc.webmanifest"] -> pure Manifest
  ["package", p] -> Package <$> either Exception.throwM pure (Witch.tryFrom p)
  ["robots.txt"] -> pure Robots
  ["static", "bootstrap.css"] -> pure Bootstrap
  _ -> Exception.throwM $ UnknownRoute.UnknownRoute texts

render :: Route -> [Text.Text]
render route = case route of
  AppleTouchIcon -> ["apple-touch-icon.png"]
  Bootstrap -> ["static", "bootstrap.css"]
  Favicon -> ["favicon.ico"]
  HealthCheck -> ["health-check"]
  Home -> []
  Manifest -> ["monadoc.webmanifest"]
  Package p -> ["package", Witch.from p]
  Robots -> ["robots.txt"]
