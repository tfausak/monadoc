module Monadoc.Action.HackageIndex.Update where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.Trans.Control as Control
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.HackageIndex.Insert as HackageIndex.Insert
import qualified Monadoc.Constant.Header as Header
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.MissingHeader as MissingHeader
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.HttpClient as Client
import qualified Monadoc.Extra.Read as Read
import qualified Monadoc.Handler.Proxy.Get as Proxy.Get
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified Witch

run :: HackageIndex.Model -> App.App ()
run hackageIndex = do
  App.Log.debug "updating hackage index"
  (oldKey, oldSize) <- do
    rows <-
      App.Sql.query
        "select blob.key, blob.size \
        \ from hackageIndex \
        \ inner join blob \
        \ on blob.key = hackageIndex.blob \
        \ where hackageIndex.key = ? \
        \ limit 1"
        [Model.key hackageIndex]
    NotFound.fromList rows
  newSize <- HackageIndex.Insert.getSize
  let start = oldSize - 1_024
      end = newSize - 1
      range = Witch.into @ByteString.ByteString . Witch.into @(Witch.UTF_8 ByteString.ByteString) $ "bytes=" <> show start <> "-" <> show end
  case compare oldSize newSize of
    GT ->
      Traced.throw
        Mismatch.Mismatch
          { Mismatch.expected = oldSize,
            Mismatch.actual = newSize
          }
    EQ -> App.Log.debug "nothing to update"
    LT -> do
      context <- Reader.ask
      App.Log.debug $ F.sformat ("new index to get:" F.%+ F.int) (newSize - oldSize)
      request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar"
      let headers = (Http.hRange, range) : Client.requestHeaders request
      Control.control $ \runInBase -> Client.withResponse (Client.ensureUserAgent request {Client.requestHeaders = headers}) (Context.manager context) $ \response -> runInBase $ do
        actualSize <- getActualSize response
        Monad.when (actualSize /= newSize) . Traced.throw $
          Mismatch.Mismatch
            { Mismatch.expected = newSize,
              Mismatch.actual = actualSize
            }
        Temp.withTempFile (Context.temporaryDirectory context) "monadoc-" $ \f h -> do
          App.Log.debug "copying old blob"
          App.Sql.withConnection $ \connection ->
            Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.from @Blob.Key oldKey) False $ \oldBlob -> IO.liftIO $ do
              contents <- Sqlite.blobRead oldBlob oldSize 0
              ByteString.hPut h contents
          App.Log.debug "appending new blob"
          IO.liftIO $ do
            IO.hSeek h IO.RelativeSeek (-1_024)
            Loops.whileJust_ (Proxy.Get.readChunk response) $ ByteString.hPut h
            IO.hClose h
          App.Log.debug "inserting new blob"
          blobKey <- upsertBlob f
          Monad.void $ HackageIndex.Insert.insertHackageIndex blobKey

getActualSize :: Client.Response a -> App.App Int
getActualSize response = do
  a <- case lookup Header.contentRange $ Client.responseHeaders response of
    Nothing -> Traced.throw $ MissingHeader.MissingHeader Header.contentRange
    Just c -> pure c
  b <- Either.throw . Witch.tryInto @Text.Text . Witch.into @(Witch.UTF_8 ByteString.ByteString) . ByteString.drop 1 . snd $ ByteString.break (== 0x2f) a
  Either.throw $ Read.tryRead b

-- | This is similar to 'Monadoc.Action.Blob.Upsert.run'. The key difference is
-- that it operates on a file rather than a byte string. And it attempts to
-- keep resident memory usage at a minimum.
upsertBlob :: FilePath -> App.App Blob.Key
upsertBlob filePath = do
  hash <-
    fmap (Witch.from @(Crypto.Digest Crypto.SHA256) . Crypto.hashlazy)
      . IO.liftIO
      $ LazyByteString.readFile filePath
  r1 <- App.Sql.query "select key from blob where hash = ? limit 1" [hash]
  case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      size <- IO.liftIO $ Directory.getFileSize filePath
      r2 <-
        App.Sql.query
          "insert into blob (size, hash, contents) values (?, ?, zeroblob(?)) returning key"
          (size, hash :: Hash.Hash, size)
      key <- case r2 of
        [] -> Traced.throw MissingKey.MissingKey
        Sql.Only key : _ -> pure key
      App.Sql.withConnection $ \connection ->
        Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.into @Int.Int64 key) True $ \blob -> IO.liftIO $ do
          contents <- ByteString.readFile filePath
          Sqlite.blobWrite blob contents 0
      pure key
