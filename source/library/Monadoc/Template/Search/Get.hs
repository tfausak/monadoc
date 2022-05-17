{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Template.Search.Get where

import qualified Lucid
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Query as Query
import qualified Monadoc.Type.Route as Route

render ::
  Context.Context ->
  Query.Query ->
  Lucid.Html ()
render context query = Common.base context (Route.Search query) "Search :: Monadoc" $ do
  Lucid.h2_ "Search"
  Lucid.p_ $
    if Query.isEmpty query
      then "You didn't search for anything, which is good since search isn't implemented yet."
      else do
        "You searched for "
        Lucid.strong_ $ Lucid.toHtml query
        " but search hasn't been implemented yet."
