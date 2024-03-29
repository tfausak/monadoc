module Monadoc.Type.RouteSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Type.Search as Search
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Route" $ do
  let packageName = Witch.unsafeFrom @String @PackageName.PackageName "pkg"
      hackageUserName = Witch.unsafeFrom @String @HackageUserName.HackageUserName "usr"
      versionNumber = Witch.unsafeFrom @String @VersionNumber.VersionNumber "1.2.3"
      revision = Witch.from @Word @Revision.Revision 4
      reversion = Reversion.Reversion {Reversion.revision = revision, Reversion.version = versionNumber}
      query = Witch.from @Text.Text @Search.Search "qry"
      componentType = ComponentType.Executable
      componentName = Witch.unsafeFrom @String @ComponentName.ComponentName "bin"
      componentId = ComponentId.ComponentId {ComponentId.type_ = componentType, ComponentId.name = componentName}
      moduleName = Witch.unsafeFrom @String @ModuleName.ModuleName "Ex.Mod"
  Hspec.it "can be parsed" $ do
    Route.parse [] [] `Hspec.shouldBe` Just Route.Home
    Route.parse ["apple-touch-icon.png"] [] `Hspec.shouldBe` Just Route.AppleTouchIcon
    Route.parse ["favicon.ico"] [] `Hspec.shouldBe` Just Route.Favicon
    Route.parse ["health-check"] [] `Hspec.shouldBe` Just Route.HealthCheck
    Route.parse ["package", "pkg"] [] `Hspec.shouldBe` Just (Route.Package packageName)
    Route.parse ["package", "pkg", "version", "1.2.3-4"] [] `Hspec.shouldBe` Just (Route.Version packageName reversion)
    Route.parse ["package", "pkg", "version", "1.2.3-4", "component", "exe:bin"] [] `Hspec.shouldBe` Just (Route.Component packageName reversion componentId)
    Route.parse ["package", "pkg", "version", "1.2.3-4", "component", "exe:bin", "module", "Ex.Mod"] [] `Hspec.shouldBe` Just (Route.Module packageName reversion componentId moduleName)
    Route.parse ["robots.txt"] [] `Hspec.shouldBe` Just Route.Robots
    Route.parse ["search"] [("query", Just $ Witch.into @ByteString.ByteString query)] `Hspec.shouldBe` Just (Route.Search query)
    Route.parse ["static", "monadoc.css"] [] `Hspec.shouldBe` Just Route.Stylesheet
    Route.parse ["static", "monadoc.js"] [] `Hspec.shouldBe` Just Route.Script
    Route.parse ["static", "monadoc.webmanifest"] [] `Hspec.shouldBe` Just Route.Manifest
    Route.parse ["user", "usr"] [] `Hspec.shouldBe` Just (Route.User hackageUserName)

  Hspec.it "can be rendered" $ do
    Route.render Route.Home `Hspec.shouldBe` ([], [])
    Route.render Route.AppleTouchIcon `Hspec.shouldBe` (["apple-touch-icon.png"], [])
    Route.render (Route.Component packageName reversion componentId) `Hspec.shouldBe` (["package", "pkg", "version", "1.2.3-4", "component", "exe:bin"], [])
    Route.render Route.Favicon `Hspec.shouldBe` (["favicon.ico"], [])
    Route.render Route.HealthCheck `Hspec.shouldBe` (["health-check"], [])
    Route.render Route.Manifest `Hspec.shouldBe` (["static", "monadoc.webmanifest"], [])
    Route.render (Route.Module packageName reversion componentId moduleName) `Hspec.shouldBe` (["package", "pkg", "version", "1.2.3-4", "component", "exe:bin", "module", "Ex.Mod"], [])
    Route.render (Route.Package packageName) `Hspec.shouldBe` (["package", "pkg"], [])
    Route.render Route.Robots `Hspec.shouldBe` (["robots.txt"], [])
    Route.render Route.Script `Hspec.shouldBe` (["static", "monadoc.js"], [])
    Route.render (Route.Search query) `Hspec.shouldBe` (["search"], [("query", Just $ Witch.into @ByteString.ByteString query)])
    Route.render Route.Stylesheet `Hspec.shouldBe` (["static", "monadoc.css"], [])
    Route.render (Route.User hackageUserName) `Hspec.shouldBe` (["user", "usr"], [])
    Route.render (Route.Version packageName reversion) `Hspec.shouldBe` (["package", "pkg", "version", "1.2.3-4"], [])
