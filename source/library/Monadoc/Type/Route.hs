module Monadoc.Type.Route where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Exception.UnknownRoute as UnknownRoute
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Type.ComponentId as ComponentId
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.ModuleName as ModuleName
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Reversion as Reversion
import qualified Monadoc.Type.Search as Search
import qualified Monadoc.Type.Url as Url
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Witch

data Route
  = AppleTouchIcon
  | Component PackageName.PackageName Reversion.Reversion ComponentId.ComponentId
  | Favicon
  | HealthCheck
  | Home
  | Manifest
  | Module PackageName.PackageName Reversion.Reversion ComponentId.ComponentId ModuleName.ModuleName
  | Package PackageName.PackageName
  | Proxy Hash.Hash Url.Url
  | Robots
  | Script
  | Search Search.Search
  | Stylesheet
  | User HackageUserName.HackageUserName
  | Version PackageName.PackageName Reversion.Reversion
  deriving (Eq, Show)

parse :: (Exception.MonadThrow m) => [Text.Text] -> Http.Query -> m Route
parse path query = case path of
  [] -> pure Home
  ["apple-touch-icon.png"] -> pure AppleTouchIcon
  ["favicon.ico"] -> pure Favicon
  ["health-check"] -> pure HealthCheck
  ["package", p] -> Package <$> tryFrom p
  ["package", p, "version", v] -> Version <$> tryFrom p <*> tryFrom v
  ["package", p, "version", v, "component", c] -> Component <$> tryFrom p <*> tryFrom v <*> tryFrom c
  ["package", p, "version", v, "component", c, "module", m] -> Module <$> tryFrom p <*> tryFrom v <*> tryFrom c <*> tryFrom m
  ["proxy", h, u] -> Proxy <$> tryFrom h <*> tryFrom u
  ["robots.txt"] -> pure Robots
  ["search"] -> Search <$> fromQuery query "query"
  ["static", "monadoc.css"] -> pure Stylesheet
  ["static", "monadoc.js"] -> pure Script
  ["static", "monadoc.webmanifest"] -> pure Manifest
  ["user", u] -> User <$> tryFrom u
  _ -> Traced.throw $ UnknownRoute.UnknownRoute path

render :: Route -> ([Text.Text], Http.Query)
render route = case route of
  AppleTouchIcon -> (["apple-touch-icon.png"], [])
  Component p v c -> (["package", Witch.from p, "version", Witch.from v, "component", Witch.from c], [])
  Favicon -> (["favicon.ico"], [])
  HealthCheck -> (["health-check"], [])
  Home -> ([], [])
  Manifest -> (["static", "monadoc.webmanifest"], [])
  Module p v c m -> (["package", Witch.from p, "version", Witch.from v, "component", Witch.from c, "module", Witch.from m], [])
  Package p -> (["package", Witch.from p], [])
  Proxy h u -> (["proxy", Witch.from h, Witch.from . Uri.escapeURIString Uri.isUnescapedInURIComponent $ Witch.from u], [])
  Robots -> (["robots.txt"], [])
  Script -> (["static", "monadoc.js"], [])
  Search q -> (["search"], if Search.isBlank q then [] else [toQuery "query" q])
  Stylesheet -> (["static", "monadoc.css"], [])
  User u -> (["user", Witch.from u], [])
  Version p v -> (["package", Witch.from p, "version", Witch.from v], [])

tryFrom ::
  ( Exception.MonadThrow m,
    Show a,
    Witch.TryFrom a b,
    Typeable.Typeable a,
    Typeable.Typeable b
  ) =>
  a ->
  m b
tryFrom = Either.throw . Witch.tryFrom

fromQuery ::
  ( Exception.MonadThrow m,
    Witch.TryFrom ByteString.ByteString a,
    Typeable.Typeable a
  ) =>
  Http.Query ->
  ByteString.ByteString ->
  m a
fromQuery query =
  tryFrom
    . Maybe.fromMaybe ByteString.empty
    . Monad.join
    . flip lookup query

toQuery ::
  (Witch.From a ByteString.ByteString) =>
  ByteString.ByteString ->
  a ->
  Http.QueryItem
toQuery k = (,) k . Just . Witch.into @ByteString.ByteString
