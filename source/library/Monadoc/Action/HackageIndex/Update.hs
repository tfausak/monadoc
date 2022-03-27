{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Update where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.HackageIndex.Insert as HackageIndex.Insert
import qualified Monadoc.Exception.InvalidSize as InvalidSize
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Say

run :: HackageIndex.Key -> Int -> App.App ()
run oldKey oldSize = do
  Say.sayString "updating hackage index"
  newSize <- HackageIndex.Insert.getSize
  let start = oldSize - 1024
      end = newSize - 1
      range = Witch.into @ByteString.ByteString $ "bytes=" <> show start <> "-" <> show end
  case compare oldSize newSize of
    GT -> Exception.throwM $ InvalidSize.InvalidSize oldSize newSize
    EQ -> Say.sayString "nothing to update"
    LT -> do
      context <- Reader.ask
      Pool.withResource (Context.pool context) $ \connection -> do
        Say.sayString $ "new index to get: " <> show (newSize - oldSize)
        request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar"
        let headers = (Http.hRange, range) : Client.requestHeaders request
        Trans.lift $ Sql.execute connection (Witch.into @Sql.Query "insert into hackageIndex (contents, size) values (zeroblob(?), ?)") (newSize, newSize)
        newKey <- Trans.lift $ Sql.selectLastInsertRowid connection
        Trans.lift . Sqlite.withBlob (Sql.connectionHandle connection) (Witch.into @Text.Text "hackageIndex") (Witch.into @Text.Text "contents") newKey True $ \newBlob -> do
          Sqlite.withBlob (Sql.connectionHandle connection) (Witch.into @Text.Text "hackageIndex") (Witch.into @Text.Text "contents") (Witch.into @Int.Int64 oldKey) False $ \oldBlob -> do
            contents <- Sqlite.blobRead oldBlob oldSize 0
            Sqlite.blobWrite newBlob contents 0
          Client.withResponse request {Client.requestHeaders = headers} (Context.manager context) $ \response -> do
            let loop offset = do
                  chunk <- Client.brRead $ Client.responseBody response
                  let size = ByteString.length chunk
                  Monad.unless (ByteString.null chunk) $ do
                    Sqlite.blobWrite newBlob chunk offset
                    loop $ offset + size
            loop start
        Trans.lift $ Sql.execute connection (Witch.into @Sql.Query "delete from hackageIndex where key != ?") [newKey]
