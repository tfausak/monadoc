{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Time
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Package as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Version as Cabal
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.Preference.Upsert as Preference.Upsert
import qualified Monadoc.Action.Range.Upsert as Range.Upsert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.UnexpectedEntry as UnexpectedEntry
import qualified Monadoc.Extra.Cabal as Cabal
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Model.Range as Range
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
  rows <- MonadSql.query_ "select * from hackageIndex where processedAt is null order by createdAt asc limit 1"
  case rows of
    [] -> MonadLog.debug "no new hackage index to process"
    hackageIndex : _ -> do
      constraints <- Base.liftBase $ Stm.newTVarIO Map.empty
      revisions <- Base.liftBase $ Stm.newTVarIO Map.empty
      context <- Reader.ask
      let blobKey = HackageIndex.blob $ Model.value hackageIndex
      size <- do
        xs <- MonadSql.query "select size from blob where key = ?" [blobKey]
        case xs of
          [] -> Exception.throwM NotFound.NotFound
          Sql.Only x : _ -> pure x
      Pool.withResource (Context.pool context) $ \connection ->
        Sqlite.withBlob (Sql.connectionHandle connection) "blob" "contents" (Witch.into @Int.Int64 blobKey) False $ \blob -> do
          contents <- Base.liftBase $ Sqlite.unsafeBlobRead blob size 0
          mapM_ (handleItem constraints revisions)
            . Tar.foldEntries ((:) . Right) [] (pure . Left)
            . Tar.read
            $ LazyByteString.fromChunks contents
      upsertPreferences constraints
      updateLatest
      now <- Timestamp.getCurrentTime
      MonadSql.execute "update hackageIndex set processedAt = ? where key = ?" (Just now, Model.key hackageIndex)

handleItem ::
  (Base.MonadBase IO m, MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Either Tar.FormatError Tar.Entry ->
  m ()
handleItem constraints =
  either Exception.throwM . handleEntry constraints

handleEntry ::
  (Base.MonadBase IO m, MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  m ()
handleEntry constraints revisions entry =
  case FilePath.splitDirectories $ Tar.entryPath entry of
    [pkg, "preferred-versions"] ->
      handlePreference constraints entry pkg
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
handlePreference constraints entry pkg = do
  packageName <-
    Either.throw $
      Witch.tryInto @PackageName.PackageName pkg
  lazyByteString <- case Tar.entryContent entry of
    Tar.NormalFile lazyByteString _ -> pure lazyByteString
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
  string <- Either.throw $ Witch.tryInto @String lazyByteString
  versionRange <-
    if null string
      then pure Cabal.anyVersion
      else do
        Cabal.PackageVersionConstraint name range <- Either.throw $ Cabal.tryParsec string
        Monad.when (name /= Witch.into @Cabal.PackageName packageName)
          . Exception.throwM
          $ UnexpectedEntry.UnexpectedEntry entry
        pure range
  Base.liftBase
    . Stm.atomically
    . Stm.modifyTVar' constraints
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
    Either.throw $
      Witch.tryInto @PackageName.PackageName pkg
  versionNumber <-
    Either.throw $
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
    Either.throw
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
          Upload.version = Model.key version,
          Upload.isPreferred = True,
          Upload.isLatest = False
        }
  Monad.when (rem (Witch.into @Int.Int64 (Model.key upload)) 10000 == 1)
    . MonadLog.debug
    . Text.pack
    $ show upload

{- hlint ignore epochTimeToPosixTime "Use from" -}
epochTimeToPosixTime :: Tar.EpochTime -> Time.POSIXTime
epochTimeToPosixTime = fromIntegral

upsertPreferences ::
  (Base.MonadBase IO m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  m ()
upsertPreferences var = do
  constraints <- Base.liftBase $ Stm.readTVarIO var
  mapM_ (uncurry upsertPreference) $ Map.toList constraints

upsertPreference ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  PackageName.PackageName ->
  Constraint.Constraint ->
  m ()
upsertPreference packageName constraint = do
  package <- Package.Upsert.run Package.Package {Package.name = packageName}
  range <- Range.Upsert.run Range.Range {Range.constraint = constraint}
  Monad.void $
    Preference.Upsert.run
      Preference.Preference
        { Preference.package = Model.key package,
          Preference.range = Model.key range
        }
  rows <- MonadSql.query "select * from upload inner join version on version.key = upload.version where upload.package = ?" [Model.key package]
  Monad.forM_ rows $ \(upload Sql.:. version) -> do
    let isPreferred = Constraint.includes (Version.number $ Model.value version) constraint
    Monad.when (Upload.isPreferred (Model.value upload) /= isPreferred) $
      MonadSql.execute "update upload set isPreferred = ? where key = ?" (isPreferred, Model.key upload)

updateLatest :: MonadSql.MonadSql m => m ()
updateLatest = do
  keys <- MonadSql.query_ "select key from package"
  Monad.forM_ keys $ \(Sql.Only key) -> do
    rows <-
      MonadSql.query
        "select * \
        \ from upload \
        \ inner join version \
        \ on version.key = upload.version \
        \ where upload.package = ?"
        [key :: Package.Key]
    let f (upload Sql.:. version) =
          Ord.Down
            ( Upload.isPreferred $ Model.value upload,
              Version.number $ Model.value version,
              Upload.revision $ Model.value upload
            )
    case Maybe.listToMaybe $ List.sortOn f rows of
      Nothing -> pure ()
      Just (upload Sql.:. _) ->
        MonadSql.execute
          "update upload set isLatest = (key = ?) where package = ?"
          (Model.key upload, key)
