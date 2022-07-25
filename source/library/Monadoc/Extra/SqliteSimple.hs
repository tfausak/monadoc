module Monadoc.Extra.SqliteSimple where

import qualified Control.Monad.Trans.Control as Control
import qualified Database.SQLite.Simple as Sql

stream ::
  (Sql.FromRow r, Sql.ToRow p) =>
  Sql.Connection ->
  Sql.Query ->
  p ->
  (r -> IO ()) ->
  IO ()
stream c q p = Sql.fold c q p () . const

streamLifted ::
  (Sql.FromRow r, Control.MonadBaseControl IO m, Sql.ToRow p) =>
  Sql.Connection ->
  Sql.Query ->
  p ->
  (r -> m ()) ->
  m ()
streamLifted c q = Control.liftBaseOpDiscard . stream c q
