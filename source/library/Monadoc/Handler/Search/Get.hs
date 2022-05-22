{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.Search.Get where

import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Template.Search.Get as Template
import qualified Monadoc.Type.Breadcrumb as Breadcrumb
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Query as Query
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

handler :: (Reader.MonadReader Context.Context m, MonadSql.MonadSql m) => Query.Query -> Wai.Request -> m Wai.Response
handler query _ = do
  context <- Reader.ask
  packages <-
    if Query.isBlank query
      then pure []
      else
        MonadSql.query
          "select * from package where name like ? escape '\\' order by name collate nocase asc limit 16"
          [like query]
  hackageUsers <-
    if Query.isBlank query
      then pure []
      else
        MonadSql.query
          "select * from hackageUser where name like ? escape '\\' order by name collate nocase asc limit 16"
          [like query]
  let breadcrumbs =
        Breadcrumb.Breadcrumb {Breadcrumb.label = "Home", Breadcrumb.route = Just Route.Home}
          : if Query.isBlank query
            then [Breadcrumb.Breadcrumb {Breadcrumb.label = "Search", Breadcrumb.route = Nothing}]
            else
              [ Breadcrumb.Breadcrumb {Breadcrumb.label = "Search", Breadcrumb.route = Just $ Route.Search Query.empty},
                Breadcrumb.Breadcrumb {Breadcrumb.label = Witch.into @Text.Text query, Breadcrumb.route = Nothing}
              ]
  pure . Common.htmlResponse Http.ok200 [] $ Template.render context breadcrumbs query packages hackageUsers

like :: Query.Query -> Text.Text
like = Text.cons '%' . flip Text.snoc '%' . escape . Witch.into @Text.Text

escape :: Text.Text -> Text.Text
escape = Text.concatMap $ \c -> case c of
  '_' -> Text.pack ['\\', c]
  '\\' -> Text.pack ['\\', c]
  '%' -> Text.pack ['\\', c]
  _ -> Text.singleton c
