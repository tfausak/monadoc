module Monadoc.Action.HackageIndex.Process where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Package as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Version as Cabal
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.Preference.Upsert as Preference.Upsert
import qualified Monadoc.Action.Range.Upsert as Range.Upsert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Exception.Traced as Traced
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
import qualified Monadoc.Query.Package as Package.Query
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified Witch

run :: App.App ()
run = do
  rows <- App.Sql.query_ @HackageIndex.Model "select * from hackageIndex where processedAt is null order by createdAt asc limit 1"
  case rows of
    [] -> App.Log.debug "no new hackage index to process"
    hackageIndex : _ -> do
      constraints <- IO.liftIO $ Stm.newTVarIO Map.empty
      revisions <- IO.liftIO $ Stm.newTVarIO Map.empty
      let blobKey = hackageIndex.value.blob
      size <- do
        xs <- App.Sql.query "select size from blob where key = ? limit 1" [blobKey]
        Sql.fromOnly <$> NotFound.fromList xs
      context <- Reader.ask
      Temp.withTempFile context.temporaryDirectory "monadoc-" $ \f h -> do
        App.Sql.withConnection $ \connection ->
          Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.into @Int.Int64 blobKey) False $ \blob -> IO.liftIO $ do
            chunks <- Sqlite.unsafeBlobRead blob size 0
            LazyByteString.hPut h $ LazyByteString.fromChunks chunks
            IO.hClose h
        contents <- IO.liftIO $ LazyByteString.readFile f
        mapM_ (handleItem constraints revisions)
          . Tar.foldEntries ((:) . Right) [] (pure . Left)
          . Tar.read
          $ case hackageIndex.value.size of
            Nothing -> contents
            Just _ -> Gzip.decompress contents
      upsertPreferences constraints
      updateLatest
      now <- Timestamp.getCurrentTime
      App.Sql.execute "update hackageIndex set processedAt = ? where key = ?" (Just now, hackageIndex.key)

handleItem ::
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Either Tar.FormatError Tar.Entry ->
  App.App ()
handleItem constraints =
  either Traced.throw . handleEntry constraints

handleEntry ::
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  App.App ()
handleEntry constraints revisions entry =
  case FilePath.splitDirectories $ Tar.entryPath entry of
    [pkg, "preferred-versions"] ->
      handlePreference constraints entry pkg
    [pkg, ver, base] -> case FilePath.splitExtensions base of
      ("package", ".json") -> handlePackageJson entry
      (p, ".cabal") | p == pkg -> handleCabal revisions entry pkg ver
      _ -> Traced.throw $ UnexpectedEntry.UnexpectedEntry entry
    _ -> Traced.throw $ UnexpectedEntry.UnexpectedEntry entry

handlePreference ::
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  Tar.Entry ->
  String ->
  App.App ()
handlePreference constraints entry pkg = do
  packageName <-
    Either.throw $
      Witch.tryInto @PackageName.PackageName pkg
  lazyByteString <- case Tar.entryContent entry of
    Tar.NormalFile lazyByteString _ -> pure lazyByteString
    _ -> Traced.throw $ UnexpectedEntry.UnexpectedEntry entry
  text <- Either.throw . Witch.tryInto @Text.Text $ Witch.into @(Witch.UTF_8 LazyByteString.ByteString) lazyByteString
  versionRange <-
    if Text.null text
      then pure Cabal.anyVersion
      else do
        Cabal.PackageVersionConstraint name range <- Either.throw $ Cabal.tryParsec text
        Monad.when (name /= Witch.into @Cabal.PackageName packageName)
          . Traced.throw
          $ UnexpectedEntry.UnexpectedEntry entry
        pure range
  IO.liftIO
    . Stm.atomically
    . Stm.modifyTVar' constraints
    . Map.insert packageName
    $ Witch.into @Constraint.Constraint versionRange

handlePackageJson :: (Applicative m) => Tar.Entry -> m ()
handlePackageJson _ = pure ()

handleCabal ::
  Stm.TVar (Map.Map (PackageName.PackageName, VersionNumber.VersionNumber) Revision.Revision) ->
  Tar.Entry ->
  String ->
  String ->
  App.App ()
handleCabal revisions entry pkg ver = do
  packageName <-
    Either.throw $
      Witch.tryInto @PackageName.PackageName pkg
  versionNumber <-
    Either.throw $
      Witch.tryInto @VersionNumber.VersionNumber ver
  let key = (packageName, versionNumber)
  revision <- IO.liftIO . Stm.atomically . Stm.stateTVar revisions $ \m ->
    let revision = Map.findWithDefault Revision.zero key m
     in (revision, Map.insert key (Revision.increment revision) m)
  byteString <- case Tar.entryContent entry of
    Tar.NormalFile lazyByteString _ ->
      pure $
        Witch.into @ByteString.ByteString lazyByteString
    _ -> Traced.throw $ UnexpectedEntry.UnexpectedEntry entry
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
  Monad.void $
    Upload.Upsert.run
      Upload.Upload
        { Upload.blob = blob.key,
          Upload.package = package.key,
          Upload.revision = revision,
          Upload.uploadedAt = Witch.from $ Tar.entryTime entry,
          Upload.uploadedBy = hackageUser.key,
          Upload.version = version.key,
          Upload.isPreferred = True,
          Upload.isLatest = False
        }

upsertPreferences ::
  Stm.TVar (Map.Map PackageName.PackageName Constraint.Constraint) ->
  App.App ()
upsertPreferences var = do
  constraints <- IO.liftIO $ Stm.readTVarIO var
  mapM_ (uncurry upsertPreference) $ Map.toList constraints

upsertPreference ::
  PackageName.PackageName ->
  Constraint.Constraint ->
  App.App ()
upsertPreference packageName constraint = do
  package <- Package.Upsert.run Package.Package {Package.name = packageName}
  range <- Range.Upsert.run Range.Range {Range.constraint = constraint}
  Monad.void $
    Preference.Upsert.run
      Preference.Preference
        { Preference.package = package.key,
          Preference.range = range.key
        }
  rows <- App.Sql.query @(Upload.Model Sql.:. Version.Model) "select * from upload inner join version on version.key = upload.version where upload.package = ?" [package.key]
  Monad.forM_ rows $ \(upload Sql.:. version) -> do
    let isPreferred = Constraint.includes version.value.number constraint
    Monad.when (upload.value.isPreferred /= isPreferred) $
      App.Sql.execute "update upload set isPreferred = ? where key = ?" (isPreferred, upload.key)

updateLatest :: App.App ()
updateLatest = do
  keys <- Package.Query.getKeys
  Monad.forM_ keys $ \key -> do
    rows <-
      App.Sql.query @(Upload.Model Sql.:. Version.Model)
        "select * \
        \ from upload \
        \ inner join version \
        \ on version.key = upload.version \
        \ where upload.package = ?"
        [key :: Package.Key]
    let f ::
          Upload.Model Sql.:. Version.Model ->
          Ord.Down (Bool, VersionNumber.VersionNumber, Revision.Revision)
        f (upload Sql.:. version) =
          Ord.Down
            ( upload.value.isPreferred,
              version.value.number,
              upload.value.revision
            )
    case Maybe.listToMaybe $ List.sortOn f rows of
      Nothing -> pure ()
      Just (upload Sql.:. _) ->
        App.Sql.execute
          "update upload set isLatest = (key = ?) where package = ?"
          (upload.key, key)
