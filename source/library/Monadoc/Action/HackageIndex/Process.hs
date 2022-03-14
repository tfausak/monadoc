{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Process where

import qualified Codec.Archive.Tar as Tar
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Version as Cabal
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified System.FilePath as FilePath
import qualified Witch

run :: HackageIndex.Model -> App.App ()
run hackageIndex = do
  preferredVersions <- App.lift $ Stm.newTVarIO Map.empty
  mapM_ (handleItem preferredVersions)
    . Tar.foldEntries ((:) . Right) [] (pure . Left)
    . Tar.read
    . Witch.into @LazyByteString.ByteString
    . HackageIndex.contents
    $ Model.value hackageIndex

-- TODO

handleItem ::
  Stm.TVar (Map.Map Cabal.PackageName Cabal.VersionRange) ->
  Either Tar.FormatError Tar.Entry ->
  App.App ()
handleItem = either Exception.throwM . handleEntry

handleEntry ::
  Stm.TVar (Map.Map Cabal.PackageName Cabal.VersionRange) ->
  Tar.Entry ->
  App.App ()
handleEntry preferredVersions entry = case FilePath.splitDirectories $ Tar.entryPath entry of
  [package, "preferred-versions"] -> do
    packageName <- case Cabal.simpleParsec @Cabal.PackageName package of
      Just packageName -> pure packageName
      Nothing -> Exception.throwM $ userError "TODO: invalid package name"
    lazyByteString <- case Tar.entryContent entry of
      Tar.NormalFile lazyByteString _ -> pure lazyByteString
      _ -> Exception.throwM $ userError "TODO: unexpected entry content"
    string <- case Witch.tryInto @String lazyByteString of
      Right string -> pure string
      Left e -> Exception.throwM e
    versionRange <- case Cabal.simpleParsec string of
      Just (Cabal.PackageVersionConstraint _ versionRange) -> pure versionRange
      Nothing
        | null string -> pure Cabal.anyVersion
        | otherwise -> Exception.throwM $ userError "TODO: invalid preferred version range"
    App.lift . Stm.atomically . Stm.modifyTVar preferredVersions $ Map.insert packageName versionRange
  _ -> pure () -- TODO
