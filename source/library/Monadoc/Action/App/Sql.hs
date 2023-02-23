module Monadoc.Action.App.Sql where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Control as Control
import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.ResourcePool as Pool
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Query as Query
import qualified Witch

withConnection ::
  (Control.MonadBaseControl IO m) =>
  (Sql.Connection -> App.AppT m a) ->
  App.AppT m a
withConnection callback = do
  context <- Reader.ask
  Pool.withResourceLifted (Context.pool context) callback

query :: (Sql.FromRow r, Sql.ToRow q) => Query.Query -> q -> App.App [r]
query q r = withConnection $ \c -> IO.liftIO $ Sql.query c (Witch.from q) r

query_ :: (Sql.FromRow r) => Query.Query -> App.App [r]
query_ q = withConnection $ \c -> IO.liftIO . Sql.query_ c $ Witch.from q

execute :: (Sql.ToRow q) => Query.Query -> q -> App.App ()
execute q r = withConnection $ \c -> IO.liftIO $ Sql.execute c (Witch.from q) r

execute_ :: Query.Query -> App.App ()
execute_ q = withConnection $ \c -> IO.liftIO . Sql.execute_ c $ Witch.from q
