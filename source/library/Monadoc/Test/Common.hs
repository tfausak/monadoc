{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Monadoc.Test.Common where

import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Control as Control
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Lazy as LazyText
import qualified Data.Time as Time
import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified GHC.Clock as Clock
import qualified GHC.Stack as Stack
import qualified Lucid
import qualified Monadoc.Action.Database.Initialize as Database.Initialize
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSay as MonadSay
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype FakeT m a = FakeT
  { runFakeT :: Reader.ReaderT Sql.Connection m a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      Base.MonadBase b,
      Control.MonadBaseControl b,
      Trans.MonadIO,
      Reader.MonadReader Sql.Connection,
      Exception.MonadThrow
    )

instance Base.MonadBase IO m => MonadLog.MonadLog (FakeT m)

instance Monad m => MonadSay.MonadSay (FakeT m) where
  hSay = const . const $ pure ()

instance Base.MonadBase IO m => MonadSql.MonadSql (FakeT m) where
  query query row = do
    connection <- Reader.ask
    Base.liftBase $ Sql.query connection query row

instance Base.MonadBase IO m => MonadTime.MonadTime (FakeT m) where
  getCurrentTime = Base.liftBase Time.getCurrentTime
  getMonotonicTime = Base.liftBase Clock.getMonotonicTime

arbitrary :: (QuickCheck.Arbitrary a, Base.MonadBase IO m) => m a
arbitrary = arbitraryWith id

arbitraryWith :: (QuickCheck.Arbitrary a, Base.MonadBase IO m) => (a -> a) -> m a
arbitraryWith f = fmap f . Base.liftBase $ QuickCheck.generate QuickCheck.arbitrary

exceptionSelector :: Hspec.Selector e
exceptionSelector = const True

expectFrom :: (Stack.HasCallStack, Eq t, Witch.From s t, Show t) => s -> t -> IO ()
expectFrom s t = Witch.from s `Hspec.shouldBe` t

expectFromJson :: (Stack.HasCallStack, Eq a, Aeson.FromJSON a, Show a) => LazyByteString.ByteString -> a -> IO ()
expectFromJson json x = Aeson.eitherDecode json `Hspec.shouldBe` Right x

expectFromSqlField :: (Stack.HasCallStack, Eq a, Sql.FromField a, Show a) => Sql.SQLData -> a -> IO ()
expectFromSqlField sql x = do
  let fromField :: Sql.FromField b => Sql.SQLData -> Sql.Ok b
      fromField = Sql.fromField . flip Sql.Field 0
  fromField sql `Hspec.shouldBe` Sql.Ok x

expectFromSqlRow :: (Stack.HasCallStack, Eq a, Sql.FromRow a, Show a) => [Sql.SQLData] -> a -> IO ()
expectFromSqlRow sql x = do
  let fromRow :: Sql.FromRow b => [Sql.SQLData] -> Sql.Ok b
      fromRow xs = State.evalStateT (Reader.runReaderT (Sql.unRP Sql.fromRow) . Sql.RowParseRO $ length xs) (0, xs)
  fromRow sql `Hspec.shouldBe` Sql.Ok x

expectHtml :: (Stack.HasCallStack, Lucid.ToHtml a) => a -> LazyText.Text -> IO ()
expectHtml x html = Lucid.renderText (Lucid.toHtml x) `Hspec.shouldBe` html

expectHtmlRaw :: (Stack.HasCallStack, Lucid.ToHtml a) => a -> LazyText.Text -> IO ()
expectHtmlRaw x html = Lucid.renderText (Lucid.toHtmlRaw x) `Hspec.shouldBe` html

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

runFake :: FakeT IO a -> Sql.Connection -> IO a
runFake = Reader.runReaderT . runFakeT . (*>) Database.Initialize.run

withConnection :: (Sql.Connection -> IO a) -> IO a
withConnection = Sql.withConnection ":memory:"
