{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monadoc.Action.Blob.UpsertSpec where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified GHC.Clock as Clock
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.Database.Initialize as Database.Initialize
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSay as MonadSay
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Blob.Upsert" . Hspec.around withConnection $ do
  Hspec.it "inserts a new blob" . runFake $ do
    let blob = Blob.new ""
    model <- Blob.Upsert.run blob
    Trans.liftIO $ model `Hspec.shouldBe` Model.Model {Model.key = Witch.from @Int 1, Model.value = blob}
  Hspec.it "updates an existing blob" . runFake $ do
    let blob = Blob.new ""
    old <- Blob.Upsert.run blob
    new <- Blob.Upsert.run blob
    Trans.liftIO $ new `Hspec.shouldBe` old
  Hspec.it "inserts two blobs" . runFake $ do
    a <- Blob.Upsert.run $ Blob.new "a"
    b <- Blob.Upsert.run $ Blob.new "b"
    Trans.liftIO $ Model.key a `Hspec.shouldNotBe` Model.key b

withConnection :: (Sql.Connection -> IO a) -> IO a
withConnection = Sql.withConnection ":memory:"

runFake :: FakeT IO a -> Sql.Connection -> IO a
runFake = Reader.runReaderT . runFakeT . (*>) Database.Initialize.run

newtype FakeT m a = FakeT
  { runFakeT :: Reader.ReaderT Sql.Connection m a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      Trans.MonadIO,
      Reader.MonadReader Sql.Connection,
      Exception.MonadThrow
    )

instance Trans.MonadIO m => MonadLog.MonadLog (FakeT m)

instance Monad m => MonadSay.MonadSay (FakeT m) where
  hSay = const . const $ pure ()

instance Trans.MonadIO m => MonadSql.MonadSql (FakeT m) where
  query query row = do
    connection <- Reader.ask
    Trans.liftIO $ Sql.query connection query row

instance Trans.MonadIO m => MonadTime.MonadTime (FakeT m) where
  getCurrentTime = Trans.liftIO Time.getCurrentTime
  getMonotonicTime = Trans.liftIO Clock.getMonotonicTime
