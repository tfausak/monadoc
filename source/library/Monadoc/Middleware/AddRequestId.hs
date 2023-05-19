module Monadoc.Middleware.AddRequestId where

import qualified Data.Vault.Lazy as Vault
import qualified Monadoc.Type.RequestId as RequestId
import qualified Network.Wai as Wai

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware key handle request respond = do
  requestId <- RequestId.random
  handle
    request
      { Wai.vault = Vault.insert key requestId $ Wai.vault request
      }
    respond
