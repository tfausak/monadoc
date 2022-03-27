module Monadoc.Type.App where

import qualified Control.Monad.Reader as Reader
import qualified Monadoc.Type.Context as Context

type App = Reader.ReaderT Context.Context IO
