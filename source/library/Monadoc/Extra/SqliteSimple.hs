{-# LANGUAGE TypeApplications #-}

module Monadoc.Extra.SqliteSimple where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Int as Int
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Exception.MissingRowid as MissingRowid
import qualified Monadoc.Vendor.Witch as Witch

selectLastInsertRowid :: Sql.Connection -> IO Int.Int64
selectLastInsertRowid connection = do
  rows <- Sql.query_ connection $ Witch.into @Sql.Query "select last_insert_rowid()"
  case rows of
    [] -> Exception.throwM MissingRowid.MissingRowid
    Sql.Only key : _ -> do
      Monad.when (key == 0) $ Exception.throwM MissingRowid.MissingRowid
      pure key
