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
import qualified Data.Map.Strict as Map
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
import qualified Monadoc.Action.PreferredVersions.Upsert as PreferredVersions.Upsert
import qualified Monadoc.Action.Release.Upsert as Release.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.MissingHackageIndex as MissingHackageIndex
import qualified Monadoc.Exception.UnexpectedEntry as UnexpectedEntry
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.Release as Release
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Monadoc.Vendor.Witch as Witch
import qualified System.FilePath as FilePath

run ::
  (Control.MonadBaseControl IO m, MonadLog.MonadLog m, Exception.MonadMask m, Reader.MonadReader Context.Context m, MonadSql.MonadSql m) =>
  m ()
run = do
  (key, size) <- do
    rows <- MonadSql.query_ "select key, size from hackageIndex order by key asc limit 1"
    case rows of
      [] -> Exception.throwM MissingHackageIndex.MissingHackageIndex
      (key, size) : _ -> pure (key, size)
  preferredVersions <- Base.liftBase $ Stm.newTVarIO Map.empty
  revisions <- Base.liftBase $ Stm.newTVarIO Map.empty
  context <- Reader.ask
  Pool.withResource (Context.pool context) $ \connection ->
    Sqlite.withBlob (Sql.connectionHandle connection) "hackageIndex" "contents" key False $ \blob -> do
      contents <- Base.liftBase $ Sqlite.unsafeBlobRead blob size 0
      mapM_ (handleItem preferredVersions revisions)
        . Tar.foldEntries ((:) . Right) [] (pure . Left)
        . Tar.read
        $ LazyByteString.fromChunks contents
  upsertPreferredVersions preferredVersions

handleItem ::
  (Base.MonadBase IO m, MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Either Tar.FormatError Tar.Entry ->
  m ()
handleItem preferredVersions =
  either Exception.throwM . handleEntry preferredVersions

handleEntry ::
  (Base.MonadBase IO m, MonadLog.MonadLog m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  m ()
handleEntry preferredVersions revisions entry =
  case FilePath.splitDirectories $ Tar.entryPath entry of
    [pkg, "preferred-versions"] ->
      handlePreferredVersions preferredVersions entry pkg
    [pkg, ver, base] -> case FilePath.splitExtensions base of
      ("package", ".json") -> handlePackageJson entry
      (p, ".cabal") | p == pkg -> handleCabal revisions entry pkg ver
      _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry

handlePreferredVersions ::
  (Base.MonadBase IO m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  Tar.Entry ->
  String ->
  m ()
handlePreferredVersions preferredVersions entry pkg = do
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
        Cabal.PackageVersionConstraint name range <-
          either Exception.throwM pure $
            Witch.tryInto @Cabal.PackageVersionConstraint string
        Monad.when (name /= Witch.into @Cabal.PackageName packageName)
          . Exception.throwM
          $ UnexpectedEntry.UnexpectedEntry entry
        pure range
  Base.liftBase . Stm.atomically . Stm.modifyTVar' preferredVersions
    . Map.insert packageName
    $ Witch.into @VersionRange.VersionRange versionRange

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
  hackageUser <-
    HackageUser.Upsert.run
      HackageUser.HackageUser
        { HackageUser.name =
            Witch.into @HackageUserName.HackageUserName
              . Tar.ownerName
              $ Tar.entryOwnership entry
        }
  release <-
    Release.Upsert.run
      Release.Release
        { Release.blob = Model.key blob,
          Release.package = Model.key package,
          Release.revision = revision,
          Release.uploadedAt =
            Time.posixSecondsToUTCTime
              . fromIntegral
              $ Tar.entryTime entry,
          Release.uploadedBy = Model.key hackageUser,
          Release.version = Model.key version
        }
  Monad.when (rem (Witch.into @Int.Int64 (Model.key release)) 10000 == 1)
    . MonadLog.info
    . Text.pack
    $ show release

upsertPreferredVersions ::
  (Base.MonadBase IO m, MonadSql.MonadSql m, Exception.MonadThrow m) =>
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  m ()
upsertPreferredVersions var = do
  preferredVersions <- Base.liftBase $ Stm.readTVarIO var
  mapM_ (uncurry upsertPreferredVersion) $ Map.toList preferredVersions

upsertPreferredVersion :: (MonadSql.MonadSql m, Exception.MonadThrow m) => PackageName.PackageName -> VersionRange.VersionRange -> m ()
upsertPreferredVersion packageName versionRange = do
  package <- Package.Upsert.run Package.Package {Package.name = packageName}
  Monad.void $
    PreferredVersions.Upsert.run
      PreferredVersions.PreferredVersions
        { PreferredVersions.package = Model.key package,
          PreferredVersions.range = versionRange
        }
