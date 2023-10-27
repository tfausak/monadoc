module Monadoc.Exception.ProxyError where

import qualified Control.Monad.Catch as Exception
import qualified Network.HTTP.Client as Client

newtype ProxyError
  = ProxyError Client.HttpException
  deriving (Show)

instance Exception.Exception ProxyError
