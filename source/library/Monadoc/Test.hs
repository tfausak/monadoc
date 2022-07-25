{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monadoc.Test where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Lazy as LazyText
import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified GHC.Stack as Stack
import qualified Lucid as Html
import qualified Monadoc.Action.Database.Initialize as Database.Initialize
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Severity as Severity
import qualified System.Environment as Environment
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

arbitrary :: (QuickCheck.Arbitrary a, IO.MonadIO m) => m a
arbitrary = arbitraryWith id

arbitraryWith :: (QuickCheck.Arbitrary a, IO.MonadIO m) => (a -> a) -> m a
arbitraryWith f = fmap f . IO.liftIO $ QuickCheck.generate QuickCheck.arbitrary

exceptionSelector :: forall e. Exception.Exception e => Hspec.Selector Exception.SomeException
exceptionSelector x
  | Just (Traced.Traced y _) <- Exception.fromException x = exceptionSelector @e y
  | Just _ <- Exception.fromException @e x = True
  | otherwise = False

expectFrom :: (Stack.HasCallStack, Eq t, Witch.From s t, Show t) => s -> t -> IO ()
expectFrom s t = Witch.from s `Hspec.shouldBe` t

expectFromJson :: (Stack.HasCallStack, Eq a, Aeson.FromJSON a, Show a) => LazyByteString.ByteString -> a -> IO ()
expectFromJson json x = Aeson.eitherDecode json `Hspec.shouldBe` Right x

expectFromSqlField :: (Stack.HasCallStack, Eq a, Sql.FromField a, Show a) => Sql.SQLData -> a -> IO ()
expectFromSqlField sql x = fromField sql `Hspec.shouldBe` Sql.Ok x

expectFromSqlRow :: (Stack.HasCallStack, Eq a, Sql.FromRow a, Show a) => [Sql.SQLData] -> a -> IO ()
expectFromSqlRow sql x = fromRow sql `Hspec.shouldBe` Sql.Ok x

expectHtml :: (Stack.HasCallStack, Html.ToHtml a) => a -> LazyText.Text -> IO ()
expectHtml x html = Html.renderText (Html.toHtml x) `Hspec.shouldBe` html

expectHtmlRaw :: (Stack.HasCallStack, Html.ToHtml a) => a -> LazyText.Text -> IO ()
expectHtmlRaw x html = Html.renderText (Html.toHtmlRaw x) `Hspec.shouldBe` html

expectJson :: (Stack.HasCallStack, Eq a, Aeson.FromJSON a, Show a, Aeson.ToJSON a) => a -> LazyByteString.ByteString -> IO ()
expectJson x json = do
  expectToJson x json
  expectFromJson json x

expectSqlField :: (Stack.HasCallStack, Eq a, Sql.FromField a, Show a, Sql.ToField a) => a -> Sql.SQLData -> IO ()
expectSqlField x sql = do
  expectToSqlField x sql
  expectFromSqlField sql x

expectSqlRow :: (Stack.HasCallStack, Eq a, Sql.FromRow a, Show a, Sql.ToRow a) => a -> [Sql.SQLData] -> IO ()
expectSqlRow x sql = do
  expectToSqlRow x sql
  expectFromSqlRow sql x

expectToJson :: (Stack.HasCallStack, Aeson.ToJSON a) => a -> LazyByteString.ByteString -> IO ()
expectToJson x json = Aeson.encode x `Hspec.shouldBe` json

expectToSqlField :: (Stack.HasCallStack, Sql.ToField a) => a -> Sql.SQLData -> IO ()
expectToSqlField x sql = Sql.toField x `Hspec.shouldBe` sql

expectToSqlRow :: (Stack.HasCallStack, Sql.ToRow a) => a -> [Sql.SQLData] -> IO ()
expectToSqlRow x sql = Sql.toRow x `Hspec.shouldBe` sql

expectTryFrom :: (Stack.HasCallStack, Eq t, Show s, Show t, Witch.TryFrom s t, Typeable.Typeable s, Typeable.Typeable t) => s -> t -> IO ()
expectTryFrom s expected = case Witch.tryFrom s of
  Left e -> Hspec.expectationFailure $ Exception.displayException e
  Right actual -> expected `Hspec.shouldBe` actual

fromField :: Sql.FromField b => Sql.SQLData -> Sql.Ok b
fromField = Sql.fromField . flip Sql.Field 0

fromRow :: Sql.FromRow b => [Sql.SQLData] -> Sql.Ok b
fromRow xs = State.evalStateT (Reader.runReaderT (Sql.unRP Sql.fromRow) . Sql.RowParseRO $ length xs) (0, xs)

propertyJson :: (Eq a, Aeson.FromJSON a, Show a, Aeson.ToJSON a) => a -> QuickCheck.Property
propertyJson x = Aeson.eitherDecode (Aeson.encode x) QuickCheck.=== Right x

propertySqlField :: (Eq a, Sql.FromField a, Show a, Sql.ToField a) => a -> QuickCheck.Property
propertySqlField x = fromField (Sql.toField x) QuickCheck.=== Sql.Ok x

propertySqlRow :: (Eq a, Sql.FromRow a, Show a, Sql.ToRow a) => a -> QuickCheck.Property
propertySqlRow x = fromRow (Sql.toRow x) QuickCheck.=== Sql.Ok x

run :: App.App a -> IO a
run app = do
  name <- Environment.getProgName
  let config =
        Config.initial
          { Config.sql = ":memory:",
            Config.severity = Severity.Error
          }
  context <- Context.fromConfig name config
  App.run context $ do
    Database.Initialize.run
    app
