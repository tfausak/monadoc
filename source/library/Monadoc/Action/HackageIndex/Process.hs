{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Process where

import qualified Codec.Archive.Tar as Tar
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Function as Function
import qualified Data.Map as Map
import qualified Distribution.Package as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Version as Cabal
import qualified Monadoc.Exception.UnexpectedEntry as UnexpectedEntry
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Revision as Revision
import qualified System.FilePath as FilePath
import qualified Witch

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

-- TODO

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
    [package, "preferred-versions"] -> do
      packageName <- case Cabal.simpleParsec @Cabal.PackageName package of
        Just packageName -> pure packageName
        Nothing -> Exception.throwM $ userError "TODO: invalid package name"
      lazyByteString <- case Tar.entryContent entry of
        Tar.NormalFile lazyByteString _ -> pure lazyByteString
        _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
      string <- case Witch.tryInto @String lazyByteString of
        Right string -> pure string
        Left e -> Exception.throwM e
      versionRange <- case Cabal.simpleParsec string of
        Just (Cabal.PackageVersionConstraint _ versionRange) -> pure versionRange
        Nothing
          | null string -> pure Cabal.anyVersion
          | otherwise -> Exception.throwM $ userError "TODO: invalid preferred version range"
      App.lift . Stm.atomically . Stm.modifyTVar preferredVersions $ Map.insert packageName versionRange
    [package, version, base] -> case FilePath.splitExtensions base of
      ("package", ".json") -> pure ()
      (p, ".cabal") | p == package -> do
        packageName <- case Cabal.simpleParsec @Cabal.PackageName package of
          Just packageName -> pure packageName
          Nothing -> Exception.throwM $ userError "TODO: invalid package name"
        ver <- case Cabal.simpleParsec @Cabal.Version version of
          Just ver -> pure ver
          Nothing -> Exception.throwM $ userError "TODO: invalid version number"
        let packageIdentifier =
              Cabal.PackageIdentifier
                { Cabal.pkgName = packageName,
                  Cabal.pkgVersion = ver
                }
        App.lift
          . Stm.atomically
          . Stm.modifyTVar' revisions
          . Map.insertWith (\x -> Witch.from @Int . Function.on (+) Witch.from x) packageIdentifier
          $ Witch.from @Int 1
        pure () -- TODO
      _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
    _ -> Exception.throwM $ UnexpectedEntry.UnexpectedEntry entry
