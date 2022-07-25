module Monadoc.Type.UrlSpec where

import qualified Data.Text as Text
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Url as Url
import qualified Network.URI as Uri
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Url" $ do
  let uri = Uri.URI "http:" (Just $ Uri.URIAuth "" "a.test" "") "/b" "?c" "#d"
      url = Witch.into @Url.Url uri
      text = "http://a.test/b?c#d" :: Text.Text

  Hspec.it "can be converted into text" $ do
    Test.expectFrom url text

  Hspec.it "can be converted from text" $ do
    Test.expectTryFrom text url
