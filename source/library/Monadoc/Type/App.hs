module Monadoc.Type.App where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Type.Context as Context

type App = AppT IO

type AppT = Reader.ReaderT Context.Context

run :: Context.Context -> AppT m a -> m a
run = flip Reader.runReaderT
