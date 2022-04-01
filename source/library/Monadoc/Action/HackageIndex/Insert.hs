{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Insert where

import qualified Codec.Compression.Zlib.Internal as Zlib
import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.ConversionFailure as ConversionFailure
import qualified Monadoc.Exception.MissingSize as MissingSize
import qualified Monadoc.Exception.TrailingBytes as TrailingBytes
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Text.Read as Read
import qualified Witch

run :: (Control.MonadBaseControl IO m, MonadHttp.MonadHttp m, MonadLog.MonadLog m, Exception.MonadMask m, Reader.MonadReader Context.Context m, MonadSql.MonadSql m) => m ()
run = do
  MonadLog.info "inserting hackage index"
  size <- getSize
  context <- Reader.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar.gz"
  MonadHttp.withResponse request $ \response -> do
    MonadSql.execute "insert into hackageIndex (contents, processedAt, size) values (zeroblob(?), null, ?)" (size, size)
    key <- Key.SelectLastInsert.run
    Pool.withResource (Context.pool context) $ \connection -> Sqlite.withBlob (Sql.connectionHandle connection) "hackageIndex" "contents" (Witch.into @Int.Int64 key) True $ \blob -> do
      offsetRef <- Base.liftBase $ IORef.newIORef 0
      Base.liftBase $
        Zlib.foldDecompressStream
          ( \f -> do
              chunk <- Client.brRead $ Client.responseBody response
              f chunk
          )
          ( \chunk io -> do
              offset <- IORef.atomicModifyIORef' offsetRef $ \n -> (n + ByteString.length chunk, n)
              Sqlite.blobWrite blob chunk offset
              io
          )
          ( \leftovers ->
              Monad.unless (ByteString.null leftovers)
                . Exception.throwM
                $ TrailingBytes.TrailingBytes leftovers
          )
          Exception.throwM
          (Zlib.decompressIO Zlib.gzipFormat Zlib.defaultDecompressParams)

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
  string <- either Exception.throwM pure $ Witch.tryInto @String byteString
  case Read.readMaybe string of
    Nothing -> Exception.throwM $ ConversionFailure.new @Int string
    Just int -> pure int
