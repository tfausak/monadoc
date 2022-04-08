module Monadoc.Test where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Lazy as LazyText
import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid
import qualified Test.Hspec as Hspec
import qualified Witch

expectJson ::
  (Eq a, Aeson.FromJSON a, Show a, Aeson.ToJSON a) =>
  a ->
  LazyByteString.ByteString ->
  Hspec.Expectation
expectJson x json = do
  expectToJson x json
  expectFromJson json x

expectToJson ::
  Aeson.ToJSON a =>
  a ->
  LazyByteString.ByteString ->
  Hspec.Expectation
expectToJson x json = Aeson.encode x `Hspec.shouldBe` json

expectFromJson ::
  (Eq a, Aeson.FromJSON a, Show a) =>
  LazyByteString.ByteString ->
  a ->
  Hspec.Expectation
expectFromJson json x = Aeson.eitherDecode json `Hspec.shouldBe` Right x

expectSqlField ::
  (Eq a, Sql.FromField a, Show a, Sql.ToField a) =>
  a ->
  Sql.SQLData ->
  Hspec.Expectation
expectSqlField x sql = do
  expectToSqlField x sql
  expectFromSqlField sql x

expectToSqlField ::
  Sql.ToField a =>
  a ->
  Sql.SQLData ->
  Hspec.Expectation
expectToSqlField x sql = Sql.toField x `Hspec.shouldBe` sql

expectFromSqlField ::
  (Eq a, Sql.FromField a, Show a) =>
  Sql.SQLData ->
  a ->
  Hspec.Expectation
expectFromSqlField sql x = do
  let fromField :: Sql.FromField b => Sql.SQLData -> Sql.Ok b
      fromField = Sql.fromField . flip Sql.Field 0
  fromField sql `Hspec.shouldBe` Sql.Ok x

expectSqlRow ::
  (Eq a, Sql.FromRow a, Show a, Sql.ToRow a) =>
  a ->
  [Sql.SQLData] ->
  Hspec.Expectation
expectSqlRow x sql = do
  expectToSqlRow x sql
  expectFromSqlRow sql x

expectToSqlRow ::
  Sql.ToRow a =>
  a ->
  [Sql.SQLData] ->
  Hspec.Expectation
expectToSqlRow x sql = Sql.toRow x `Hspec.shouldBe` sql

expectFromSqlRow ::
  (Eq a, Sql.FromRow a, Show a) =>
  [Sql.SQLData] ->
  a ->
  Hspec.Expectation
expectFromSqlRow sql x = do
  let fromRow :: Sql.FromRow b => [Sql.SQLData] -> Sql.Ok b
      fromRow xs =
        State.evalStateT
          (Reader.runReaderT (Sql.unRP Sql.fromRow) . Sql.RowParseRO $ length xs)
          (0, xs)
  fromRow sql `Hspec.shouldBe` Sql.Ok x

expectFrom ::
  (Eq t, Witch.From s t, Show t) =>
  s ->
  t ->
  Hspec.Expectation
expectFrom s t = Witch.from s `Hspec.shouldBe` t

expectTryFrom ::
  ( Eq t,
    Show s,
    Show t,
    Witch.TryFrom s t,
    Typeable.Typeable s,
    Typeable.Typeable t
  ) =>
  s ->
  t ->
  Hspec.Expectation
expectTryFrom s expected = case Witch.tryFrom s of
  Left e -> Hspec.expectationFailure $ Exception.displayException e
  Right actual -> expected `Hspec.shouldBe` actual

expectHtml ::
  Lucid.ToHtml a =>
  a ->
  LazyText.Text ->
  Hspec.Expectation
expectHtml x html = Lucid.renderText (Lucid.toHtml x) `Hspec.shouldBe` html

expectHtmlRaw ::
  Lucid.ToHtml a =>
  a ->
  LazyText.Text ->
  Hspec.Expectation
expectHtmlRaw x html = Lucid.renderText (Lucid.toHtmlRaw x) `Hspec.shouldBe` html

thisException :: Hspec.Selector e
thisException = const True
