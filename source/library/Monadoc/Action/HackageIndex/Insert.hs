{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Insert where

import qualified Codec.Compression.Zlib.Internal as Zlib
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Exception.MissingSize as MissingSize
import qualified Monadoc.Exception.ReadFailure as ReadFailure
import qualified Monadoc.Exception.TrailingBytes as TrailingBytes
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.SqliteSimple as Sql
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Say
import qualified Text.Read as Read

run :: App.App ()
run = do
  Say.sayString "inserting hackage index"
  context <- Reader.ask
  App.withConnection $ \connection -> do
    size <- getSize
    request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar.gz"
    Trans.lift . Client.withResponse request (Context.manager context) $ \response -> do
      Sql.execute connection (Witch.into @Sql.Query "insert into hackageIndex (contents, size) values (zeroblob(?), ?)") (size, size)
      key <- Sql.selectLastInsertRowid connection
      Sqlite.withBlob (Sql.connectionHandle connection) (Witch.into @Text.Text "hackageIndex") (Witch.into @Text.Text "contents") key True $ \blob -> do
        offsetRef <- IORef.newIORef 0
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

getSize :: App.App Int
getSize = do
  context <- Reader.ask
  request <-
    Client.parseUrlThrow $
      Config.hackage (Context.config context) <> "01-index.tar"
  response <-
    Trans.lift
      . Client.httpNoBody request {Client.method = Http.methodHead}
      $ Context.manager context
  byteString <-
    maybe (Exception.throwM $ MissingSize.MissingSize response) pure
      . lookup Http.hContentLength
      $ Client.responseHeaders response
  string <- either Exception.throwM pure $ Witch.tryInto @String byteString
  either (Exception.throwM . ReadFailure.ReadFailure string) pure $ Read.readEither string
