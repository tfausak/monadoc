{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Type.Route where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute

data Route
  = AppleTouchIcon
  | Bootstrap
  | Favicon
  | HealthCheck
  | Home
  | Manifest
  | Robots
  deriving (Eq, Show)

parse :: Exception.MonadThrow m => [Text.Text] -> m Route
parse texts = case texts of
  [] -> pure Home
  ["apple-touch-icon.png"] -> pure AppleTouchIcon
  ["favicon.ico"] -> pure Favicon
  ["health-check"] -> pure HealthCheck
  ["monadoc.webmanifest"] -> pure Manifest
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
  Robots -> ["robots.txt"]
