module Monadoc.Type.RouteSpec where

import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Route as Route
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Route" $ do
  let packageName = Witch.unsafeFrom @String @PackageName.PackageName "example"
  Hspec.it "can be parsed" $ do
    Route.parse [] `Hspec.shouldBe` Just Route.Home
    Route.parse ["apple-touch-icon.png"] `Hspec.shouldBe` Just Route.AppleTouchIcon
    Route.parse ["favicon.ico"] `Hspec.shouldBe` Just Route.Favicon
    Route.parse ["health-check"] `Hspec.shouldBe` Just Route.HealthCheck
    Route.parse ["monadoc.webmanifest"] `Hspec.shouldBe` Just Route.Manifest
    Route.parse ["package", "example"] `Hspec.shouldBe` Just (Route.Package packageName)
    Route.parse ["robots.txt"] `Hspec.shouldBe` Just Route.Robots
    Route.parse ["static", "bootstrap.css"] `Hspec.shouldBe` Just Route.Bootstrap

  Hspec.it "can be rendered" $ do
    Route.render Route.Home `Hspec.shouldBe` []
    Route.render Route.AppleTouchIcon `Hspec.shouldBe` ["apple-touch-icon.png"]
    Route.render Route.Favicon `Hspec.shouldBe` ["favicon.ico"]
    Route.render Route.HealthCheck `Hspec.shouldBe` ["health-check"]
    Route.render Route.Manifest `Hspec.shouldBe` ["monadoc.webmanifest"]
    Route.render (Route.Package packageName) `Hspec.shouldBe` ["package", "example"]
    Route.render Route.Robots `Hspec.shouldBe` ["robots.txt"]
    Route.render Route.Bootstrap `Hspec.shouldBe` ["static", "bootstrap.css"]
