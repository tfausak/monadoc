{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Insert where

import qualified Codec.Compression.GZip as Gzip
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Exception.MissingSize as MissingSize
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Text.Read as Read

run :: App.App ()
run = do
  context <- App.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar.gz"
  response <- App.httpLbs request
  let contents = Witch.into @ByteString.ByteString . Gzip.decompress $ Client.responseBody response
      hackageIndex =
        HackageIndex.HackageIndex
          { HackageIndex.contents = contents,
            HackageIndex.size = ByteString.length contents
          }
  App.withConnection $ \connection ->
    App.lift $ Sql.execute connection (Witch.from "insert into hackageIndex (contents, size) values (?, ?)") hackageIndex

-- TODO: Use this to populate new row in hackageIndex table.
getSize :: App.App Int
getSize = do
  context <- App.ask
  request <-
    Client.parseUrlThrow $
      Config.hackage (Context.config context) <> "01-index.tar.gz"
  response <-
    App.lift
      . Client.httpNoBody request {Client.method = Http.methodHead}
      $ Context.manager context
  byteString <-
    maybe (Exception.throwM $ MissingSize.MissingSize response) pure
      . lookup Http.hContentLength
      $ Client.responseHeaders response
  string <- either Exception.throwM pure $ Witch.tryInto @String byteString
  either (Exception.throwM . userError) pure $ Read.readEither string
