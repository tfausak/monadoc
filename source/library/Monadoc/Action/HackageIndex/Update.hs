{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Update where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
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

run :: HackageIndex.Model -> App.App HackageIndex.Model
run hackageIndex = do
  context <- App.ask
  request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "/01-index.tar"
  headResponse <- App.httpLbs request {Client.method = Http.methodHead}
  newSize <- maybe (Exception.throwM $ MissingSize.MissingSize headResponse) pure $ do
    byteString <- lookup Http.hContentLength $ Client.responseHeaders headResponse
    string <- either (const Nothing) Just $ Witch.tryInto @String byteString
    Read.readMaybe @Int string
  let oldSize = HackageIndex.size $ Model.value hackageIndex
      start = oldSize - 1024
      end = newSize - 1
      range = Witch.into @ByteString.ByteString $ "bytes=" <> show start <> "-" <> show end
  case compare oldSize newSize of
    GT -> Exception.throwM $ InvalidSize.InvalidSize oldSize newSize
    EQ -> pure hackageIndex
    LT -> do
      rangeResponse <-
        App.httpLbs
          request
            { Client.requestHeaders = (Http.hRange, range) : Client.requestHeaders request
            }
      let before = ByteString.take start . HackageIndex.contents $ Model.value hackageIndex
          after = Witch.into @ByteString.ByteString $ Client.responseBody rangeResponse
          contents = before <> after
          size = ByteString.length contents
      App.withConnection $ \connection ->
        App.lift $
          Sql.execute
            connection
            (Witch.from "update hackageIndex set contents = ?, size = ? where key = ?")
            (contents, size, Model.key hackageIndex)
      pure
        hackageIndex
          { Model.value =
              HackageIndex.HackageIndex
                { HackageIndex.contents = contents,
                  HackageIndex.size = size
                }
          }
