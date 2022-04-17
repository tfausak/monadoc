module Monadoc.Action.HackageIndex.Process where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Map.Strict as Map
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Time
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Package as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Version as Cabal
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.Preference.Upsert as Preference.Upsert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.ConversionFailure as ConversionFailure
import qualified Monadoc.Exception.UnexpectedEntry as UnexpectedEntry
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified System.FilePath as FilePath
import qualified Witch

run ::
  ( Control.MonadBaseControl IO m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSql.MonadSql m
  ) =>
  m ()
run = do
  rows <- MonadSql.query_ "select key, size from hackageIndex where updatedAt is not null and processedAt is null order by createdAt asc limit 1"
  case rows of
    [] -> MonadLog.debug "no new hackage index to process"
    (key, size) : _ -> do
      preference <- Base.liftBase $ Stm.newTVarIO Map.empty
      revisions <- Base.liftBase $ Stm.newTVarIO Map.empty
      context <- Reader.ask
      Pool.withResource (Context.pool context) $ \connection ->
        Sqlite.withBlob (Sql.connectionHandle connection) "hackageIndex" "contents" key False $ \blob -> do
          contents <- Base.liftBase $ Sqlite.unsafeBlobRead blob size 0
          mapM_ (handleItem preference revisions)
            . Tar.foldEntries ((:) . Right) [] (pure . Left)
            . Tar.read
            $ LazyByteString.fromChunks contents
      upsertPreference preference
      now <- Timestamp.getCurrentTime
      MonadSql.execute "update hackageIndex set processedAt = ? where key = ?" (Just now, key)

handleItem ::
  (Base.MonadBase IO m, MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Either Tar.FormatError Tar.Entry ->
  m ()
handleItem preference =
  either Exception.throwM . handleEntry preference

handleEntry ::
  (Base.MonadBase IO m, MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  m ()
handleEntry preference revisions entry =
  case FilePath.splitDirectories $ Tar.entryPath entry of
    [pkg, "preferred-versions"] ->
      handlePreference preference entry pkg
    [pkg, ver, base] -> case FilePath.splitExtensions base of
      ("package", ".json") -> handlePackageJson entry
      (p, ".cabal") | p == pkg -> handleCabal revisions entry pkg ver
      _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry

handlePreference ::
  (Base.MonadBase IO m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Tar.Entry ->
  String ->
  m ()
handlePreference preference entry pkg = do
  packageName <-
    either Exception.throwM pure $
      Witch.tryInto @PackageName.PackageName pkg
  lazyByteString <- case Tar.entryContent entry of
    Tar.NormalFile lazyByteString _ -> pure lazyByteString
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
  string <- either Exception.throwM pure $ Witch.tryInto @String lazyByteString
  versionRange <-
    if null string
      then pure Cabal.anyVersion
      else do
        Cabal.PackageVersionConstraint name range <- case Cabal.simpleParsec string of
          Nothing -> Exception.throwM $ ConversionFailure.new @Cabal.PackageVersionConstraint string
          Just x -> pure x
        Monad.when (name /= Witch.into @Cabal.PackageName packageName)
          . Exception.throwM
          $ UnexpectedEntry.UnexpectedEntry entry
        pure range
  Base.liftBase . Stm.atomically . Stm.modifyTVar' preference
    . Map.insert packageName
    $ Witch.into @Constraint.Constraint versionRange

handlePackageJson :: Applicative m => Tar.Entry -> m ()
handlePackageJson _ = pure ()

handleCabal ::
  (Base.MonadBase IO m, MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  String ->
  String ->
  m ()
handleCabal revisions entry pkg ver = do
  packageName <-
    either Exception.throwM pure $
      Witch.tryInto @PackageName.PackageName pkg
  versionNumber <-
    either Exception.throwM pure $
      Witch.tryInto @VersionNumber.VersionNumber ver
  let key = (packageName, versionNumber)
  revision <- Base.liftBase . Stm.atomically . Stm.stateTVar revisions $ \m ->
    let revision = Map.findWithDefault Revision.zero key m
     in (revision, Map.insert key (Revision.increment revision) m)
  byteString <- case Tar.entryContent entry of
    Tar.NormalFile lazyByteString _ ->
      pure $
        Witch.into @ByteString.ByteString lazyByteString
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
  blob <- Blob.Upsert.run $ Blob.new byteString
  package <- Package.Upsert.run Package.Package {Package.name = packageName}
  version <- Version.Upsert.run Version.Version {Version.number = versionNumber}
  hackageUserName <-
    either Exception.throwM pure
      . Witch.tryInto @HackageUserName.HackageUserName
      . Tar.ownerName
      $ Tar.entryOwnership entry
  hackageUser <-
    HackageUser.Upsert.run
      HackageUser.HackageUser
        { HackageUser.name = hackageUserName
        }
  upload <-
    Upload.Upsert.run
      Upload.Upload
        { Upload.blob = Model.key blob,
          Upload.package = Model.key package,
          Upload.revision = revision,
          Upload.uploadedAt =
            Witch.from
              . Time.posixSecondsToUTCTime
              . epochTimeToPosixTime
              $ Tar.entryTime entry,
          Upload.uploadedBy = Model.key hackageUser,
          Upload.version = Model.key version
        }
  Monad.when (rem (Witch.into @Int.Int64 (Model.key upload)) 10000 == 1)
    . MonadLog.debug
    . Text.pack
    $ show upload

{- hlint ignore epochTimeToPosixTime "Use from" -}
epochTimeToPosixTime :: Tar.EpochTime -> Time.POSIXTime
epochTimeToPosixTime = fromIntegral

upsertPreference ::
  (Base.MonadBase IO m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  m ()
upsertPreference var = do
  preference <- Base.liftBase $ Stm.readTVarIO var
  mapM_ (uncurry upsertPreferredVersion) $ Map.toList preference

upsertPreferredVersion :: (MonadSql.MonadSql m, Exception.MonadThrow m) => PackageName.PackageName -> Constraint.Constraint -> m ()
upsertPreferredVersion packageName constraint = do
  package <- Package.Upsert.run Package.Package {Package.name = packageName}
  Monad.void $
    Preference.Upsert.run
      Preference.Preference
        { Preference.package = Model.key package,
          Preference.constraint = constraint
        }
