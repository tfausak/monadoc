module Monadoc.Type.ComponentIdSpec where

import qualified Data.Text as Text
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.ComponentName as ComponentName
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.ComponentId" $ do
  let ct = ComponentType.Library
      cn = Witch.unsafeFrom @String @ComponentName.ComponentName "pkg"
      ci = ComponentId.ComponentId {ComponentId.type_ = ct, ComponentId.name = cn}

  Hspec.it "can be converted into text" $ do
    Test.expectFrom ci ("lib:pkg" :: Text.Text)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml ci "lib:pkg"
    Test.expectHtmlRaw ci "lib:pkg"

  Hspec.it "can be converted from text" $ do
    Test.expectTryFrom ("lib:pkg" :: Text.Text) ci
