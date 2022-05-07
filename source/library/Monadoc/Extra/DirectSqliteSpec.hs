{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Extra.DirectSqliteSpec where

import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Extra.DirectSqlite as Extra
import qualified Monadoc.Test.Common as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.DirectSqlite" . Hspec.around withDatabase $ do
  Hspec.describe "unsafeBlobRead" $ do
    Hspec.it "successfully reads a blob" $ \db -> do
      Sqlite.exec db "create table t (c blob)"
      Sqlite.exec db "insert into t (c) values (x'6873')"
      Extra.withBlob db "t" "c" 1 False $ \blob -> do
        bs <- Extra.unsafeBlobRead blob 2 0
        bs `Hspec.shouldBe` ["hs"]

  Hspec.describe "withBlob" $ do
    Hspec.it "throws an error when the table doesn't exist" $ \db -> do
      Extra.withBlob db "t" "c" 1 False (const $ pure ())
        `Hspec.shouldThrow` Test.exceptionSelector @Sqlite.SQLError

    Hspec.it "throws an error when the column doesn't exist" $ \db -> do
      Sqlite.exec db "create table t (key integer primary key)"
      Extra.withBlob db "t" "c" 1 False (const $ pure ())
        `Hspec.shouldThrow` Test.exceptionSelector @Sqlite.SQLError

    Hspec.it "throws an error when the key doesn't exist" $ \db -> do
      Sqlite.exec db "create table t (key integer primary key, c blob)"
      Extra.withBlob db "t" "c" 1 False (const $ pure ())
        `Hspec.shouldThrow` Test.exceptionSelector @Sqlite.SQLError

    Hspec.it "successfully reads a blob" $ \db -> do
      Sqlite.exec db "create table t (c blob)"
      Sqlite.exec db "insert into t (c) values (x'6873')"
      Extra.withBlob db "t" "c" 1 False $ \blob -> do
        bs <- Sqlite.blobRead blob 2 0
        bs `Hspec.shouldBe` "hs"

    Hspec.it "throws an error when writing a read-only blob" $ \db -> do
      Sqlite.exec db "create table t (c blob)"
      Sqlite.exec db "insert into t (c) values (x'')"
      Extra.withBlob db "t" "c" 1 False (\blob -> Sqlite.blobWrite blob "" 0)
        `Hspec.shouldThrow` Test.exceptionSelector @Sqlite.SQLError

    Hspec.it "successfully writes a blob" $ \db -> do
      Sqlite.exec db "create table t (c blob)"
      Sqlite.exec db "insert into t (c) values (x'0000')"
      Extra.withBlob db "t" "c" 1 True $ \blob -> do
        Sqlite.blobWrite blob "hs" 0
        bs <- Sqlite.blobRead blob 2 0
        bs `Hspec.shouldBe` "hs"

withDatabase :: (Sqlite.Database -> IO a) -> IO a
withDatabase = Exception.bracket (Sqlite.open ":memory:") Sqlite.close
