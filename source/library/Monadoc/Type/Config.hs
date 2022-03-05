module Monadoc.Type.Config where

import qualified Data.String as String
import qualified Network.Wai.Handler.Warp as Warp

data Config = Config
  { database :: String,
    help :: Bool,
    host :: Warp.HostPreference,
    port :: Warp.Port,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { database = "monadoc.sqlite",
      help = False,
      host = String.fromString "127.0.0.1",
      port = 3000,
      version = False
    }
