module Monadoc.Exception.MissingCode where

import qualified Control.Monad.Catch as Exception
import qualified Network.Wai as Wai

newtype MissingCode
    = MissingCode Wai.Request
    deriving Show

instance Exception.Exception MissingCode

new :: Wai.Request -> MissingCode
new = MissingCode
