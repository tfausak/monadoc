module Monadoc.Action.HackageIndex.Insert where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.Trans.Control as Control
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingHeader as MissingHeader
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.HttpClient as Client
import qualified Monadoc.Extra.Maybe as Maybe
import qualified Monadoc.Extra.Read as Read
import qualified Monadoc.Handler.Proxy.Get as Proxy.Get
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Witch

run :: App.App ()
run = do
  App.Log.debug "inserting hackage index"
  uncompressedSize <- getSize
  context <- Reader.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar.gz"
  Control.control $ \runInBase ->
    Client.withResponse (Client.ensureUserAgent request) (Context.manager context) $ \response -> runInBase $ do
      compressedSize <- getContentLength response
      key <- insertBlob compressedSize
      hashVar <- IO.liftIO . Stm.newTVarIO $ Crypto.hashInitWith Crypto.SHA256
      offsetVar <- IO.liftIO $ Stm.newTVarIO 0
      App.Sql.withConnection $ \connection ->
        Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.into @Int.Int64 key) True $ \blob -> IO.liftIO $ do
          Loops.whileJust_ (Proxy.Get.readChunk response) $ \chunk -> do
            offset <- Stm.atomically $ do
              Stm.modifyTVar' hashVar $ flip Crypto.hashUpdate chunk
              Stm.stateTVar offsetVar $ \n -> (n, n + ByteString.length chunk)
            Sqlite.blobWrite blob chunk offset
      hash <- IO.liftIO $ Stm.readTVarIO hashVar
      App.Sql.execute
        "update blob set hash = ? where key = ?"
        (Witch.into @Hash.Hash $ Crypto.hashFinalize hash, key)
      insertHackageIndex key $ Just uncompressedSize

insertHackageIndex :: Blob.Key -> Maybe Int -> App.App ()
insertHackageIndex blob size = do
  now <- Timestamp.getCurrentTime
  App.Sql.execute
    "insert into hackageIndex (blob, createdAt, processedAt, size) values (?, ?, ?, ?)"
    HackageIndex.HackageIndex
      { HackageIndex.blob = blob,
        HackageIndex.createdAt = now,
        HackageIndex.processedAt = Nothing,
        HackageIndex.size = size
      }

insertBlob :: Int -> App.App Blob.Key
insertBlob size = do
  now <- Timestamp.getCurrentTime
  let hash = Hash.new . Witch.via @(Witch.UTF_8 ByteString.ByteString) $ show now
  rows <-
    App.Sql.query
      "insert into blob (size, hash, contents) values (?, ?, zeroblob(?)) returning key"
      (size, hash, size)
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure key

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
  getContentLength response

getContentLength :: (Exception.MonadThrow m) => Client.Response a -> m Int
getContentLength response = do
  byteString <-
    Either.throw
      . Maybe.note (MissingHeader.MissingHeader Http.hContentLength)
      . lookup Http.hContentLength
      $ Client.responseHeaders response
  text <-
    Either.throw
      . Witch.tryInto @Text.Text
      $ Witch.into @(Witch.UTF_8 ByteString.ByteString) byteString
  Either.throw $ Read.tryRead text
