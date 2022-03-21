{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Process where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Map as Map
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
import qualified Monadoc.Exception.UnexpectedEntry as UnexpectedEntry
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Model.Release as Release
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.HackageUserId as HackageUserId
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Monadoc.Type.VersionRange as VersionRange
import qualified Monadoc.Vendor.Witch as Witch
import qualified System.FilePath as FilePath

run :: App.App ()
run = do
  [hackageIndex] <- App.withConnection $ \connection ->
    App.lift . Sql.query_ connection $ Witch.into @Sql.Query "select * from hackageIndex"
  preferredVersions <- App.lift $ Stm.newTVarIO Map.empty
  revisions <- App.lift $ Stm.newTVarIO Map.empty
  mapM_ (handleItem preferredVersions revisions)
    . Tar.foldEntries ((:) . Right) [] (pure . Left)
    . Tar.read
    . Witch.into @LazyByteString.ByteString
    . HackageIndex.contents
    $ Model.value hackageIndex
  upsertPreferredVersions preferredVersions

handleItem ::
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Either Tar.FormatError Tar.Entry ->
  App.App ()
handleItem preferredVersions =
  either Exception.throwM . handleEntry preferredVersions

handleEntry ::
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  App.App ()
handleEntry preferredVersions revisions entry =
  case FilePath.splitDirectories $ Tar.entryPath entry of
    [pkg, "preferred-versions"] ->
      handlePreferredVersions preferredVersions entry pkg
    [pkg, ver, base] -> case FilePath.splitExtensions base of
      ("package", ".json") -> handlePackageJson
      (p, ".cabal") | p == pkg -> handleCabal revisions entry pkg ver
      _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry

handlePreferredVersions ::
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  Tar.Entry ->
  String ->
  App.App ()
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
  App.lift . Stm.atomically . Stm.modifyTVar preferredVersions
    . Map.insert packageName
    $ Witch.into @VersionRange.VersionRange versionRange

handlePackageJson :: App.App ()
handlePackageJson = pure ()

handleCabal ::
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  String ->
  String ->
  App.App ()
handleCabal revisions entry pkg ver = do
  packageName <-
    either Exception.throwM pure $
      Witch.tryInto @PackageName.PackageName pkg
  versionNumber <-
    either Exception.throwM pure $
      Witch.tryInto @VersionNumber.VersionNumber ver
  let key = (packageName, versionNumber)
  revision <- App.lift . Stm.atomically . Stm.stateTVar revisions $ \m ->
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
        { HackageUser.id =
            Witch.into @HackageUserId.HackageUserId
              . Tar.ownerId
              $ Tar.entryOwnership entry,
          HackageUser.name =
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
  Monad.when (rem (Witch.into @Int.Int64 (Model.key release)) 100000 == 1)
    . App.sayString
    $ show release

upsertPreferredVersions ::
  Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange) ->
  App.App ()
upsertPreferredVersions var = do
  preferredVersions <- App.lift $ Stm.readTVarIO var
  mapM_ (uncurry upsertPreferredVersion) $ Map.toList preferredVersions

upsertPreferredVersion :: PackageName.PackageName -> VersionRange.VersionRange -> App.App ()
upsertPreferredVersion packageName versionRange = do
  [Sql.Only package] <- App.withConnection $ \connection ->
    App.lift $
      Sql.query
        connection
        (Witch.into @Sql.Query "select key from package where name = ?")
        [packageName]
  Monad.void $
    PreferredVersions.Upsert.run
      PreferredVersions.PreferredVersions
        { PreferredVersions.package = package,
          PreferredVersions.range = versionRange
        }
