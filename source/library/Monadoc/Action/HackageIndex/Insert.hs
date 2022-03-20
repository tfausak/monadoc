{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Insert where

import qualified Codec.Compression.GZip as Gzip
import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Client as Client

run :: App.App HackageIndex.Model
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
    App.lift $ do
      Sql.execute connection (Witch.from "insert into hackageIndex (contents, size) values (?, ?)") hackageIndex
      [Sql.Only key] <- Sql.query_ connection $ Witch.from "select key from hackageIndex"
      pure $ Model.Model {Model.key = key, Model.value = hackageIndex}
