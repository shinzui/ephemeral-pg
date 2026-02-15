-- | initdb caching for fast PostgreSQL startup.
--
-- This module provides caching of initialized PostgreSQL data directories.
-- The first startup runs initdb, subsequent startups copy from the cache.
--
-- Cache structure:
--
-- @
-- ~/.ephemeral-pg/
--   cache/
--     \<pg-version\>-\<config-hash\>/
--       data/           -- Cached initdb output
--       metadata.json   -- Cache metadata
-- @
module EphemeralPg.Internal.Cache
  ( -- * Cache operations
    CacheKey (..),
    CacheConfig (..),
    defaultCacheConfig,
    getCacheKey,
    getCacheDirectory,
    isCached,
    createCache,
    restoreFromCache,
    clearCache,
    clearAllCaches,
    cleanupRuntimeFiles,

    -- * Cache directory management
    ensureCacheDirectory,
    getCacheRoot,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Hashable (hash)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import EphemeralPg.Config (Config (..))
import EphemeralPg.Internal.CopyOnWrite
  ( CowCapability (..),
    copyDirectory,
    detectCowCapability,
  )
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.Typed (byteStringOutput, proc, readProcess, setStderr, setStdout)

-- | A cache key uniquely identifies a cached initdb cluster.
data CacheKey = CacheKey
  { -- | PostgreSQL major version (e.g., "17")
    pgVersion :: Text,
    -- | Hash of configuration that affects initdb output
    configHash :: Text
  }
  deriving stock (Eq, Show)

-- | Configuration for the cache system.
data CacheConfig = CacheConfig
  { -- | Root directory for cache (default: ~/.ephemeral-pg)
    root :: Maybe FilePath,
    -- | Copy-on-write capability (detected if Nothing)
    cow :: Maybe CowCapability,
    -- | Whether to use caching at all
    enabled :: Bool
  }
  deriving stock (Eq, Show)

-- | Default cache configuration.
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
  CacheConfig
    { root = Nothing,
      cow = Nothing,
      enabled = True
    }

-- | Get the cache root directory.
--
-- Uses XDG_CACHE_HOME if available, otherwise ~/.cache/ephemeral-pg
getCacheRoot :: Maybe FilePath -> IO FilePath
getCacheRoot mRoot = case mRoot of
  Just r -> pure r
  Nothing -> getXdgDirectory XdgCache "ephemeral-pg"

-- | Ensure the cache directory structure exists.
ensureCacheDirectory :: Maybe FilePath -> IO FilePath
ensureCacheDirectory mRoot = do
  r <- getCacheRoot mRoot
  let cacheDir = r </> "cache"
  createDirectoryIfMissing True cacheDir
  pure cacheDir

-- | Get the PostgreSQL version.
getPostgresVersion :: IO (Either Text Text)
getPostgresVersion = do
  result <- try $ readProcess config
  pure $ case result of
    Left (ex :: SomeException) ->
      Left $ "Failed to get postgres version: " <> T.pack (show ex)
    Right (ExitSuccess, stdout, _stderr) ->
      Right $ extractMajorVersion $ T.decodeUtf8Lenient $ LBS.toStrict stdout
    Right (ExitFailure code, _, _) ->
      Left $ "postgres --version failed with code " <> T.pack (show code)
  where
    config =
      proc "postgres" ["--version"]
        & setStdout byteStringOutput
        & setStderr byteStringOutput

    -- Extract major version from "postgres (PostgreSQL) 17.7"
    extractMajorVersion :: Text -> Text
    extractMajorVersion line =
      let parts = T.words line
          -- Find the version number (last word that contains a digit)
          versionPart = case filter (T.any (`elem` ['0' .. '9'])) parts of
            [] -> "unknown"
            vs -> last vs
          -- Take just the major version (before first dot)
          majorVersion = T.takeWhile (/= '.') versionPart
       in majorVersion

-- | Generate a cache key for the given configuration.
getCacheKey :: Config -> IO (Either Text CacheKey)
getCacheKey config = runExceptT $ do
  version <- ExceptT getPostgresVersion
  -- Hash the configuration elements that affect initdb output
  let configStr =
        T.unlines
          [ T.unwords config.initDbArgs,
            T.unlines $ map (\(k, v) -> k <> "=" <> v) config.postgresSettings,
            config.user
          ]
  let cfgHash = T.pack $ show $ abs $ hash (T.unpack configStr)
  pure
    CacheKey
      { pgVersion = version,
        configHash = cfgHash
      }

-- | Get the directory path for a given cache key.
getCacheDirectory :: CacheKey -> Maybe FilePath -> IO FilePath
getCacheDirectory key mRoot = do
  cacheDir <- ensureCacheDirectory mRoot
  let keyDir = T.unpack key.pgVersion <> "-" <> T.unpack key.configHash
  pure $ cacheDir </> keyDir

-- | Check if a cache exists for the given key.
isCached :: CacheKey -> Maybe FilePath -> IO Bool
isCached key mRoot = do
  dir <- getCacheDirectory key mRoot
  let dataDir = dir </> "data"
  doesDirectoryExist dataDir

-- | Create a cache from an initialized data directory.
createCache :: CacheKey -> FilePath -> Maybe FilePath -> IO (Either Text ())
createCache key srcDataDir mRoot = do
  dir <- getCacheDirectory key mRoot
  createDirectoryIfMissing True dir
  let dstDataDir = dir </> "data"

  -- Detect CoW capability
  cowCapability <- detectCowCapability dir

  -- Copy the data directory to the cache
  copyDirectory cowCapability srcDataDir dstDataDir

-- | Restore from cache to a new data directory.
restoreFromCache :: CacheKey -> FilePath -> Maybe FilePath -> IO (Either Text ())
restoreFromCache key dstDataDir mRoot = runExceptT $ do
  dir <- liftIO $ getCacheDirectory key mRoot
  let srcDataDir = dir </> "data"
  exists <- liftIO $ doesDirectoryExist srcDataDir
  unless exists $
    throwE $
      "Cache not found: " <> T.pack srcDataDir
  cowCapability <- liftIO $ detectCowCapability dir
  ExceptT $ copyDirectory cowCapability srcDataDir dstDataDir

-- | Clear the cache for a specific key.
clearCache :: CacheKey -> Maybe FilePath -> IO (Either Text ())
clearCache key mRoot = runExceptT $ do
  dir <- liftIO $ getCacheDirectory key mRoot
  exists <- liftIO $ doesDirectoryExist dir
  when exists $
    tryE "Failed to clear cache" $
      removeDirectoryRecursive dir

-- | Clear all caches.
clearAllCaches :: Maybe FilePath -> IO (Either Text ())
clearAllCaches mRoot = runExceptT $ do
  cacheDir <- liftIO $ ensureCacheDirectory mRoot
  tryE "Failed to clear all caches" $ do
    entries <- listDirectory cacheDir
    mapM_ (\e -> removeDirectoryRecursive (cacheDir </> e)) entries

-- | Try an IO action, converting exceptions to a prefixed error.
tryE :: Text -> IO a -> ExceptT Text IO a
tryE prefix action = do
  result <- liftIO $ try action
  case result of
    Left (ex :: SomeException) ->
      throwE $ prefix <> ": " <> T.pack (show ex)
    Right a -> pure a

-- | Clean up PostgreSQL runtime files from a data directory.
--
-- This removes files that are created when postgres is running
-- and should not be present in a fresh data directory (or cache).
-- Files removed:
--   - postmaster.pid (postgres process ID file)
--   - postmaster.opts (command line options)
cleanupRuntimeFiles :: FilePath -> IO ()
cleanupRuntimeFiles dataDir = do
  let runtimeFiles =
        [ dataDir </> "postmaster.pid",
          dataDir </> "postmaster.opts"
        ]
  mapM_ removeIfExists runtimeFiles
  where
    removeIfExists :: FilePath -> IO ()
    removeIfExists fp = do
      exists <- doesFileExist fp
      if exists
        then removeFile fp `catch_` pure ()
        else pure ()

    catch_ :: IO a -> IO a -> IO a
    catch_ action fallback = do
      result <- try @SomeException action
      case result of
        Left _ -> fallback
        Right a -> pure a
