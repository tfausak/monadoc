module Monadoc.Type.Handler where

import qualified Monadoc.Type.App as App
import qualified Network.Wai as Wai

type Handler =
  Wai.Request ->
  (Wai.Response -> App.App Wai.ResponseReceived) ->
  App.App Wai.ResponseReceived
