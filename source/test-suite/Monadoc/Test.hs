module Monadoc.Test where

import qualified Control.Monad.Catch as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Test.Hspec as Hspec
import qualified Witch

expectJson ::
  (Eq json, Aeson.FromJSON json, Show json, Aeson.ToJSON json) =>
  json ->
  LazyByteString.ByteString ->
  Hspec.Expectation
expectJson x json = do
  expectToJson x json
  expectFromJson json x

expectToJson ::
  Aeson.ToJSON toJson =>
  toJson ->
  LazyByteString.ByteString ->
  Hspec.Expectation
expectToJson x json = Aeson.encode x `Hspec.shouldBe` json

expectFromJson ::
  (Eq fromJson, Aeson.FromJSON fromJson, Show fromJson) =>
  LazyByteString.ByteString ->
  fromJson ->
  Hspec.Expectation
expectFromJson json x = Aeson.eitherDecode json `Hspec.shouldBe` Right x

expectSql ::
  (Eq sql, Sql.FromField sql, Show sql, Sql.ToField sql) =>
  sql ->
  Sql.SQLData ->
  Hspec.Expectation
expectSql x sql = do
  expectToSql x sql
  expectFromSql sql x

expectToSql ::
  Sql.ToField toSql =>
  toSql ->
  Sql.SQLData ->
  Hspec.Expectation
expectToSql x sql = Sql.toField x `Hspec.shouldBe` sql

expectFromSql ::
  (Eq fromSql, Sql.FromField fromSql, Show fromSql) =>
  Sql.SQLData ->
  fromSql ->
  Hspec.Expectation
expectFromSql sql x = Sql.fromField (Sql.Field sql 0) `Hspec.shouldBe` Sql.Ok x

expectFrom ::
  (Eq target, Witch.From source target, Show target) =>
  source ->
  target ->
  Hspec.Expectation
expectFrom s t = Witch.from s `Hspec.shouldBe` t

expectTryFrom ::
  ( Eq target,
    Show source,
    Show target,
    Witch.TryFrom source target,
    Typeable.Typeable source,
    Typeable.Typeable target
  ) =>
  source ->
  target ->
  Hspec.Expectation
expectTryFrom s expected = case Witch.tryFrom s of
  Left e -> Hspec.expectationFailure $ Exception.displayException e
  Right actual -> expected `Hspec.shouldBe` actual
