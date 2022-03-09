module Monadoc.Type.App where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Type.Context as Context

type App = Reader.ReaderT Context.Context IO

ask :: App Context.Context
ask = Reader.ask

lift :: IO a -> App a
lift = Trans.lift

run :: App a -> Context.Context -> IO a
run = Reader.runReaderT
