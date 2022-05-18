{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Template.Search.Get where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Lucid
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Template.Common as Common
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Query as Query
import qualified Monadoc.Type.Route as Route
import qualified Witch

render ::
  Context.Context ->
  Query.Query ->
  [Package.Model] ->
  [HackageUser.Model] ->
  Lucid.Html ()
render context query packages hackageUsers = Common.base context (Route.Search query) "Search :: Monadoc" $ do
  Lucid.h2_ "Search"
  Lucid.form_ [Lucid.action_ . Common.route context . Route.Search $ Witch.from @Text.Text "", Lucid.class_ "d-flex mb-3"] $ do
    Lucid.input_ [Lucid.class_ "form-control me-1", Lucid.name_ "query", Lucid.placeholder_ "traverse", Lucid.type_ "search", Lucid.value_ $ Witch.into @Text.Text query]
    Lucid.button_ [Lucid.class_ "btn btn-primary", Lucid.type_ "submit"] "Search"
  Lucid.h3_ "Packages"
  if null packages
    then Lucid.p_ "None found."
    else Lucid.ul_ . Monad.forM_ packages $ \package -> do
      let name = Package.name $ Model.value package
      Lucid.li_
        . Lucid.a_ [Lucid.href_ . Common.route context $ Route.Package name]
        $ Lucid.toHtml name
  Lucid.h3_ "Users"
  if null hackageUsers
    then Lucid.p_ "None found."
    else Lucid.ul_ . Monad.forM_ hackageUsers $ \hackageUser -> do
      let name = HackageUser.name $ Model.value hackageUser
      Lucid.li_
        . Lucid.a_ [Lucid.href_ . Common.route context $ Route.User name]
        $ Lucid.toHtml name
