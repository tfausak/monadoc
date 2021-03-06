{-# LANGUAGE TypeApplications #-}

module Monadoc.Job.FetchDistribution where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Catch as Exception
import qualified Control.Retry as Retry
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.Distribution as Distribution
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.PackageName as PackageName
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Version as Version
import qualified Monadoc.Vendor.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Witch

run
    :: Context.Context
    -> Map.Map (PackageName.PackageName, Version.Version) Sha256.Sha256
    -> (PackageName.PackageName, Version.Version)
    -> IO ()
run context hashes (package, version) =
    case Map.lookup (package, version) hashes of
        Just _ -> pure ()
        Nothing -> do
            let pv = Witch.into @String package <> "-" <> Witch.into @String version
            request <- Client.parseUrlThrow
                $ Config.hackageUrl (Context.config context)
                <> "/package/" <> pv <> "/" <> pv <> ".tar.gz"
            response <- Exception.catch
                (Retry.recovering
                    Retry.retryPolicyDefault
                    [const . Exception.Handler $ \ httpException -> pure $ case httpException of
                        Client.HttpExceptionRequest _ Client.ResponseTimeout -> True
                        _ -> False]
                    . const $ Client.performRequest (Context.manager context) request)
                (\ httpException -> case httpException of
                    Client.HttpExceptionRequest _ (Client.StatusCodeException response _) ->
                        case Http.statusCode $ Client.responseStatus response of
                            410 -> pure response { Client.responseBody = Gzip.compress $ Tar.write [] }
                            451 -> pure response { Client.responseBody = Gzip.compress $ Tar.write [] }
                            _ -> Exception.throwM httpException
                    _ -> Exception.throwM httpException)
            Concurrent.threadDelay 1000000
            let
                blob = Blob.fromByteString . Witch.into @ByteString.ByteString $ Client.responseBody response
                distribution = Distribution.Distribution
                    { Distribution.hash = Blob.hash blob
                    , Distribution.package = package
                    , Distribution.unpackedAt = Nothing
                    , Distribution.version = version
                    }
            Context.withConnection context $ \ connection -> do
                Blob.upsert connection blob
                Distribution.upsert connection distribution
