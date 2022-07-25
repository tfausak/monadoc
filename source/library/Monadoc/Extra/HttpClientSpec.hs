module Monadoc.Extra.HttpClientSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Monadoc.Extra.HttpClient as Client
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.HttpClient" $ do
  Hspec.describe "ensureUserAgent" $ do
    Hspec.it "adds a user agent if missing" $ do
      let headers = Client.requestHeaders $ Client.ensureUserAgent Client.defaultRequest
      lookup Http.hUserAgent headers `Hspec.shouldSatisfy` Maybe.isJust

    Hspec.it "does not change an existing user agent" $ do
      let userAgent = "my-custom-user-agent" :: ByteString.ByteString
          headers =
            Client.requestHeaders $
              Client.ensureUserAgent
                Client.defaultRequest
                  { Client.requestHeaders = [(Http.hUserAgent, userAgent)]
                  }
      lookup Http.hUserAgent headers `Hspec.shouldBe` Just userAgent
