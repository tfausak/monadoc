{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Process where

import qualified Codec.Archive.Tar as Tar
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Distribution.Package as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Version as Cabal
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Exception.UnexpectedEntry as UnexpectedEntry
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Vendor.Witch as Witch
import qualified System.FilePath as FilePath

run :: HackageIndex.Model -> App.App ()
run hackageIndex = do
  preferredVersions <- App.lift $ Stm.newTVarIO Map.empty
  revisions <- App.lift $ Stm.newTVarIO Map.empty
  mapM_ (handleItem preferredVersions revisions)
    . Tar.foldEntries ((:) . Right) [] (pure . Left)
    . Tar.read
    . Witch.into @LazyByteString.ByteString
    . HackageIndex.contents
    $ Model.value hackageIndex
  pure () -- TODO

handleItem ::
  Stm.TVar (Map.Map Cabal.PackageName Cabal.VersionRange) ->
  Stm.TVar (Map.Map Cabal.PackageId Revision.Revision) ->
  Either Tar.FormatError Tar.Entry ->
  App.App ()
handleItem preferredVersions =
  either Exception.throwM . handleEntry preferredVersions

handleEntry ::
  Stm.TVar (Map.Map Cabal.PackageName Cabal.VersionRange) ->
  Stm.TVar (Map.Map Cabal.PackageId Revision.Revision) ->
  Tar.Entry ->
  App.App ()
handleEntry preferredVersions revisions entry =
  case FilePath.splitDirectories $ Tar.entryPath entry of
    [pkg, "preferred-versions"] -> do
      packageName <- either Exception.throwM pure $ Witch.tryInto @Cabal.PackageName pkg
      lazyByteString <- case Tar.entryContent entry of
        Tar.NormalFile lazyByteString _ -> pure lazyByteString
        _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
      string <- either Exception.throwM pure $ Witch.tryInto @String lazyByteString
      versionRange <- case Witch.tryInto @Cabal.PackageVersionConstraint string of
        Right (Cabal.PackageVersionConstraint _ versionRange) -> pure versionRange
        Left tryFromException
          | null string -> pure Cabal.anyVersion
          | otherwise -> Exception.throwM tryFromException
      App.lift . Stm.atomically . Stm.modifyTVar preferredVersions $ Map.insert packageName versionRange
    [pkg, ver, base] -> case FilePath.splitExtensions base of
      ("package", ".json") -> pure ()
      (p, ".cabal") | p == pkg -> do
        packageName <- either Exception.throwM pure $ Witch.tryInto @Cabal.PackageName pkg
        version <- either Exception.throwM pure $ Witch.tryInto @Cabal.Version ver
        let packageIdentifier =
              Cabal.PackageIdentifier
                { Cabal.pkgName = packageName,
                  Cabal.pkgVersion = version
                }
        revision <- App.lift . Stm.atomically . Stm.stateTVar revisions $ \m ->
          let revision = Map.findWithDefault Revision.zero packageIdentifier m
           in (revision, Map.insert packageIdentifier (Revision.increment revision) m)
        byteString <- case Tar.entryContent entry of
          Tar.NormalFile lazyByteString _ -> pure $ Witch.into @ByteString.ByteString lazyByteString
          _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
        blob <- Blob.Upsert.run $ Blob.new byteString
        let hash = show . Blob.hash $ Model.value blob
        Monad.when (List.isPrefixOf "Hash 0000" hash)
          . App.sayString
          $ unwords [show packageName, show version, show revision, hash]
        pure () -- TODO
      _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
