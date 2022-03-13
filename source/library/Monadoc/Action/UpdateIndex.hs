{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.UpdateIndex where

import qualified Codec.Compression.GZip as Gzip
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Exception.InvalidSize as InvalidSize
import qualified Monadoc.Exception.MissingSize as MissingSize
import qualified Monadoc.Model.HackageIndex as HackageIndex
import Monadoc.Orphanage ()
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Text.Read as Read
import qualified Witch

run :: App.App ()
run = do
  App.lift $ putStrLn "beginning index"
  context <- App.ask
  hackageIndex <- Pool.withResource (Context.pool context) $ \connection ->
    fmap Maybe.listToMaybe
      . App.lift
      . Sql.query_ connection
      $ Witch.from "select * from hackageIndex"
  maybe insert update hackageIndex
  App.lift $ putStrLn "finished index"

insert :: App.App ()
insert = do
  App.lift $ putStrLn "inserting new index"
  context <- App.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "/01-index.tar.gz"
  response <- App.lift . Client.httpLbs request $ Context.manager context
  App.lift $ putStrLn "got index"
  let contents = Witch.into @ByteString.ByteString . Gzip.decompress $ Client.responseBody response
      hackageIndex =
        HackageIndex.HackageIndex
          { HackageIndex.contents = contents,
            HackageIndex.size = ByteString.length contents
          }
  App.lift . print $ HackageIndex.size hackageIndex
  Pool.withResource (Context.pool context) $ \connection ->
    App.lift $ Sql.execute connection (Witch.from "insert into hackageIndex (contents, size) values (?, ?)") hackageIndex

update :: HackageIndex.Model -> App.App ()
update hackageIndex = do
  App.lift $ putStrLn "updating existing index"
  context <- App.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "/01-index.tar"
  headResponse <- App.lift . Client.httpLbs request {Client.method = Http.methodHead} $ Context.manager context
  newSize <- maybe (Exception.throwM $ MissingSize.MissingSize headResponse) pure $ do
    byteString <- lookup Http.hContentLength $ Client.responseHeaders headResponse
    string <- either (const Nothing) Just $ Witch.tryInto @String byteString
    Read.readMaybe @Int string
  let oldSize = HackageIndex.size $ Model.value hackageIndex
      delta = newSize - oldSize
      start = oldSize - 1024
      end = newSize - 1
      range = Witch.into @ByteString.ByteString $ "bytes=" <> show start <> "-" <> show end
  Monad.when (delta < 0) . Exception.throwM $ InvalidSize.InvalidSize oldSize newSize
  Monad.when (delta > 0) $ do
    rangeResponse <-
      App.lift
        . Client.httpLbs request {Client.requestHeaders = (Http.hRange, range) : Client.requestHeaders request}
        $ Context.manager context
    let before = ByteString.take start . HackageIndex.contents $ Model.value hackageIndex
        after = Witch.into @ByteString.ByteString $ Client.responseBody rangeResponse
        contents = before <> after
        size = ByteString.length contents
    Pool.withResource (Context.pool context) $ \connection ->
      App.lift $
        Sql.execute
          connection
          (Witch.from "update hackageIndex set contents = ?, size = ? where key = ?")
          (contents, size, Model.key hackageIndex)
