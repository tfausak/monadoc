module Monadoc.Middleware.ServeStaticFiles where

import qualified Data.List as List
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Static
import qualified System.FilePath as FilePath

middleware :: Static.CacheContainer -> FilePath -> Wai.Middleware
middleware cacheContainer filePath =
  Static.staticPolicyWithOptions
    (options cacheContainer)
    (policy filePath)

options :: Static.CacheContainer -> Static.Options
options cacheContainer =
  Static.defaultOptions
    { Static.cacheContainer = cacheContainer
    }

policy :: FilePath -> Static.Policy
policy filePath =
  foldr
    (Static.<|>)
    (Static.policy $ const Nothing)
    [ Static.only
        [ ("apple-touch-icon.png", FilePath.combine filePath "apple-touch-icon.png"),
          ("favicon.ico", FilePath.combine filePath "favicon.ico"),
          ("static/monadoc.css", FilePath.combine filePath "bootstrap.css"),
          ("static/monadoc.js", FilePath.combine filePath "monadoc.js")
        ],
      Static.policy $ \string -> do
        relative <- List.stripPrefix "static/mathjax/" string
        pure $ FilePath.joinPath [filePath, "mathjax", relative]
    ]
