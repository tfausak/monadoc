{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Route where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Query as Query
import qualified Monadoc.Type.Reversion as Reversion
import qualified Network.HTTP.Types as Http
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
  | Search Query.Query
  | Stylesheet
  | User HackageUserName.HackageUserName
  | Version PackageName.PackageName Reversion.Reversion
  deriving (Eq, Show)

parse :: Exception.MonadThrow m => [Text.Text] -> Http.Query -> m Route
parse path query = case path of
  [] -> pure Home
  ["apple-touch-icon.png"] -> pure AppleTouchIcon
  ["favicon.ico"] -> pure Favicon
  ["health-check"] -> pure HealthCheck
  ["package", p] -> Package <$> Either.throw (Witch.tryFrom p)
  ["package", p, "version", v] -> Version <$> Either.throw (Witch.tryFrom p) <*> Either.throw (Witch.tryFrom v)
  ["robots.txt"] -> pure Robots
  ["search"] -> Search <$> (Either.throw . Witch.tryFrom . Maybe.fromMaybe ByteString.empty . Monad.join $ lookup "query" query)
  ["static", "monadoc.css"] -> pure Stylesheet
  ["static", "monadoc.js"] -> pure Script
  ["static", "monadoc.webmanifest"] -> pure Manifest
  ["user", u] -> User <$> Either.throw (Witch.tryFrom u)
  _ -> Exception.throwM $ UnknownRoute.UnknownRoute path

render :: Route -> ([Text.Text], Http.Query)
render route = case route of
  AppleTouchIcon -> (["apple-touch-icon.png"], [])
  Favicon -> (["favicon.ico"], [])
  HealthCheck -> (["health-check"], [])
  Home -> ([], [])
  Manifest -> (["static", "monadoc.webmanifest"], [])
  Package p -> (["package", Witch.from p], [])
  Robots -> (["robots.txt"], [])
  Script -> (["static", "monadoc.js"], [])
  Search q -> (["search"], if Query.isBlank q then [] else [("query", Just $ Witch.into @ByteString.ByteString q)])
  Stylesheet -> (["static", "monadoc.css"], [])
  User u -> (["user", Witch.from u], [])
  Version p v -> (["package", Witch.from p, "version", Witch.from v], [])
