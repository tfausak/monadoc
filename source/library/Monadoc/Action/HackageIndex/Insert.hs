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
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.MissingSize as MissingSize
import qualified Monadoc.Exception.TrailingBytes as TrailingBytes
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.Read as Read
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
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
    createdAt <- Timestamp.getCurrentTime
    MonadSql.execute "insert into hackageIndex (contents, createdAt, processedAt, size, updatedAt) values (zeroblob(?), ?, null, ?, null)" (size, createdAt, size)
    key <- Key.SelectLastInsert.run
    Pool.withResource (Context.pool context) $ \connection -> Sqlite.withBlob (Sql.connectionHandle connection) "hackageIndex" "contents" (Witch.into @Int.Int64 key) True $ \blob -> do
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
              io
          )
          ( \leftovers ->
              Monad.when (not $ ByteString.null leftovers)
                . Exception.throwM
                $ TrailingBytes.TrailingBytes leftovers
          )
          Exception.throwM
          (Zlib.decompressIO Zlib.gzipFormat Zlib.defaultDecompressParams)
    updatedAt <- Timestamp.getCurrentTime
    MonadSql.execute "update hackageIndex set updatedAt = ? where key = ?" (updatedAt, key)

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
