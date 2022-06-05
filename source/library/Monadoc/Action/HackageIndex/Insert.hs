{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Insert where

import qualified Codec.Compression.Zlib.Internal as Zlib
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Exception.MissingSize as MissingSize
import qualified Monadoc.Exception.TrailingBytes as TrailingBytes
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.Read as Read
import qualified Monadoc.Extra.ResourcePool as Pool
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Witch

run ::
  ( Control.MonadBaseControl IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSql.MonadSql m
  ) =>
  m ()
run = do
  MonadLog.debug "inserting hackage index"
  size <- getSize
  context <- Reader.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar.gz"
  MonadHttp.withResponse request $ \response -> do
    key <- insertBlob size
    Pool.liftResource (Context.pool context) $ \connection -> do
      hashVar <- Base.liftBase . Stm.newTVarIO $ Crypto.hashInitWith Crypto.SHA256
      Sqlite.withBlob (Sql.connectionHandle connection) "blob" "contents" (Witch.into @Int.Int64 key) True $ \blob -> do
        offsetRef <- Base.liftBase $ Stm.newTVarIO 0
        Base.liftBase $
          Zlib.foldDecompressStream
            ( \f -> do
                chunk <- Client.brRead $ Client.responseBody response
                f chunk
            )
            ( \chunk io -> do
                offset <- Stm.atomically . Stm.stateTVar offsetRef $ \n -> (n, n + ByteString.length chunk)
                Sqlite.blobWrite blob chunk offset
                Base.liftBase . Stm.atomically . Stm.modifyTVar' hashVar $ flip Crypto.hashUpdate chunk
                io
            )
            ( \leftovers ->
                Monad.when (not $ ByteString.null leftovers)
                  . Exception.throwM
                  $ TrailingBytes.TrailingBytes leftovers
            )
            Exception.throwM
            (Zlib.decompressIO Zlib.gzipFormat Zlib.defaultDecompressParams)
      hash <- Base.liftBase $ Stm.readTVarIO hashVar
      MonadSql.execute
        "update blob set hash = ? where key = ?"
        (Witch.into @Hash.Hash $ Crypto.hashFinalize hash, key)
    Monad.void $ insertHackageIndex key

insertHackageIndex ::
  (MonadSql.MonadSql m, Exception.MonadThrow m, MonadTime.MonadTime m) =>
  Blob.Key ->
  m HackageIndex.Model
insertHackageIndex blob = do
  now <- Timestamp.getCurrentTime
  let hackageIndex =
        HackageIndex.HackageIndex
          { HackageIndex.blob = blob,
            HackageIndex.createdAt = now,
            HackageIndex.processedAt = Nothing
          }
  MonadSql.execute
    "insert into hackageIndex (blob, createdAt, processedAt) values (?, ?, null)"
    (HackageIndex.blob hackageIndex, HackageIndex.createdAt hackageIndex)
  key <- Key.SelectLastInsert.run
  pure
    Model.Model
      { Model.key = key,
        Model.value = hackageIndex
      }

insertBlob :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Int -> m Blob.Key
insertBlob size = do
  MonadSql.execute
    "insert into blob (size, hash, contents) values (?, ?, zeroblob(?))"
    (size, Hash.new . Witch.into @ByteString.ByteString $ show size, size)
  Key.SelectLastInsert.run

getSize :: (MonadHttp.MonadHttp m, Reader.MonadReader Context.Context m, Exception.MonadThrow m) => m Int
getSize = do
  context <- Reader.ask
  request <-
    Client.parseUrlThrow $
      Config.hackage (Context.config context) <> "01-index.tar"
  response <- MonadHttp.httpNoBody request {Client.method = Http.methodHead}
  byteString <-
    maybe (Exception.throwM $ MissingSize.MissingSize response) pure
      . lookup Http.hContentLength
      $ Client.responseHeaders response
  string <- Either.throw $ Witch.tryInto @String byteString
  Either.throw $ Read.tryRead string
