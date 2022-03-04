module Monadoc.Type.Context where

import qualified Monadoc.Type.Config as Config
import qualified Network.HTTP.Client as Client

data Context = Context
  { config :: Config.Config,
    manager :: Client.Manager
  }
