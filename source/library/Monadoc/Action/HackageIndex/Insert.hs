{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Insert where

import qualified Codec.Compression.Zlib.Internal as Zlib
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Control as Control
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Exception.MissingSize as MissingSize
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Exception.TrailingBytes as TrailingBytes
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.HttpClient as Client
import qualified Monadoc.Extra.Read as Read
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Witch

run :: App.App ()
run = do
  Log.debug "inserting hackage index"
  size <- getSize
  context <- Reader.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar.gz"
  Control.control $ \runInBase -> Client.withResponse (Client.ensureUserAgent request) (Context.manager context) $ \response -> runInBase $ do
    key <- insertBlob size
    App.withConnection $ \connection -> do
      hashVar <- IO.liftIO . Stm.newTVarIO $ Crypto.hashInitWith Crypto.SHA256
      Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.into @Int.Int64 key) True $ \blob -> do
        offsetRef <- IO.liftIO $ Stm.newTVarIO 0
        IO.liftIO $
          Zlib.foldDecompressStream
            ( \f -> do
                chunk <- Client.brRead $ Client.responseBody response
                f chunk
            )
            ( \chunk io -> do
                offset <- Stm.atomically . Stm.stateTVar offsetRef $ \n -> (n, n + ByteString.length chunk)
                Sqlite.blobWrite blob chunk offset
                IO.liftIO . Stm.atomically . Stm.modifyTVar' hashVar $ flip Crypto.hashUpdate chunk
                io
            )
            ( \leftovers ->
                Monad.when (not $ ByteString.null leftovers)
                  . Traced.throw
                  $ TrailingBytes.TrailingBytes leftovers
            )
            Traced.throw
            (Zlib.decompressIO Zlib.gzipFormat Zlib.defaultDecompressParams)
      hash <- IO.liftIO $ Stm.readTVarIO hashVar
      App.execute
        "update blob set hash = ? where key = ?"
        (Witch.into @Hash.Hash $ Crypto.hashFinalize hash, key)
    Monad.void $ insertHackageIndex key

insertHackageIndex :: Blob.Key -> App.App HackageIndex.Model
insertHackageIndex blob = do
  now <- Timestamp.getCurrentTime
  let hackageIndex =
        HackageIndex.HackageIndex
          { HackageIndex.blob = blob,
            HackageIndex.createdAt = now,
            HackageIndex.processedAt = Nothing
          }
  App.execute
    "insert into hackageIndex (blob, createdAt, processedAt) values (?, ?, null)"
    (HackageIndex.blob hackageIndex, HackageIndex.createdAt hackageIndex)
  key <- Key.SelectLastInsert.run
  pure
    Model.Model
      { Model.key = key,
        Model.value = hackageIndex
      }

insertBlob :: Int -> App.App Blob.Key
insertBlob size = do
  now <- IO.liftIO Time.getCurrentTime
  App.execute
    "insert into blob (size, hash, contents) values (?, ?, zeroblob(?))"
    (size, Hash.new . Witch.into @ByteString.ByteString $ show now, size)
  Key.SelectLastInsert.run

getSize :: App.App Int
getSize = do
  context <- Reader.ask
  request <-
    Client.parseUrlThrow $
      Config.hackage (Context.config context) <> "01-index.tar"
  response <-
    Traced.wrap
      . IO.liftIO
      . Client.httpNoBody (Client.ensureUserAgent request) {Client.method = Http.methodHead}
      $ Context.manager context
  byteString <-
    maybe (Traced.throw $ MissingSize.MissingSize response) pure
      . lookup Http.hContentLength
      $ Client.responseHeaders response
  string <- Either.throw $ Witch.tryInto @String byteString
  Either.throw $ Read.tryRead string
