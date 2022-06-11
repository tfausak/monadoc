{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Key.SelectLastInsertSpec where

import qualified Control.Monad.Base as Base
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.MissingRowid as MissingRowid
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Key as Key
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Key.SelectLastInsert" . Hspec.around Test.withConnection $ do
  Hspec.it "throws if the row ID is zero" . Test.runFake $ do
    MonadSql.execute_ "create table t ( c integer primary key )"
    MonadSql.execute_ "insert into t ( c ) values ( 0 )"
    Control.control $ \runInBase ->
      runInBase Key.SelectLastInsert.run
        `Hspec.shouldThrow` Test.exceptionSelector @MissingRowid.MissingRowid

  Hspec.it "returns the row ID if it's non-zero" . Test.runFake $ do
    MonadSql.execute_ "create table t ( c integer primary key )"
    MonadSql.execute_ "insert into t ( c ) values ( 1 )"
    rowid <- Key.SelectLastInsert.run
    Base.liftBase $ rowid `Hspec.shouldBe` Witch.from @Int @(Key.Key ()) 1
