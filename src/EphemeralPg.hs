-- | Temporary PostgreSQL databases for testing.
--
-- This module provides functions for creating isolated, temporary PostgreSQL
-- instances for testing purposes. Databases are automatically cleaned up
-- when they go out of scope.
--
-- = Quick Start
--
-- @
-- import EphemeralPg qualified as Pg
-- import Hasql.Connection qualified as Connection
--
-- main :: IO ()
-- main = do
--   result <- Pg.'with' $ \\db -> do
--     Right conn <- Connection.acquire (Pg.'connectionSettings' db)
--     -- Use the connection...
--     Connection.release conn
--   case result of
--     Left err -> putStrLn $ "Error: " <> show err
--     Right () -> putStrLn "Success!"
-- @
--
-- = Custom Configuration
--
-- @
-- import EphemeralPg qualified as Pg
--
-- main :: IO ()
-- main = do
--   let config = Pg.'defaultConfig' { Pg.databaseName = "testdb" }
--   Pg.'withConfig' config $ \\db -> do
--     -- Use the database...
--     pure ()
-- @
module EphemeralPg
  ( -- * Database Handle
    Database (..),
    connectionSettings,
    connectionString,

    -- * Lifecycle Management
    with,
    withConfig,
    withCached,
    start,
    startCached,
    stop,
    restart,

    -- * Configuration
    Config (..),
    DirectoryConfig (..),
    ShutdownMode (..),
    defaultConfig,
    verboseConfig,
    autoExplainConfig,

    -- * Cache Management
    CacheConfig (..),
    defaultCacheConfig,
    clearCache,
    clearAllCaches,

    -- * Errors
    StartError (..),
    StopError (..),
    renderStartError,
    renderStopError,
  )
where

import Control.Exception (mask, onException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Word (Word16)
import EphemeralPg.Config
  ( Config (..),
    DirectoryConfig (..),
    ShutdownMode (..),
    autoExplainConfig,
    defaultConfig,
    defaultShutdownTimeoutSeconds,
    verboseConfig,
  )
import EphemeralPg.Database
  ( Database (..),
    connectionSettings,
    connectionString,
  )
import EphemeralPg.Error
  ( StartError (..),
    StopError (..),
    renderStartError,
    renderStopError,
  )
import EphemeralPg.Internal.Cache
  ( CacheConfig (..),
    CacheKey,
    cleanupRuntimeFiles,
    clearAllCaches,
    clearCache,
    createCache,
    defaultCacheConfig,
    getCacheKey,
    isCached,
    restoreFromCache,
  )
import EphemeralPg.Internal.Directory
  ( createTempDataDirectory,
    createTempSocketDirectory,
    removeDirectoryIfExists,
    resolveDirectory,
    retryRemoveDirectory,
  )
import EphemeralPg.Internal.Except (liftE, onError, runStartup)
import EphemeralPg.Internal.Port (findFreePort)
import EphemeralPg.Process (getCurrentUser)
import EphemeralPg.Process.CreateDb (runCreateDb)
import EphemeralPg.Process.InitDb (runInitDb, writePostgresConf)
import EphemeralPg.Process.Postgres (startPostgres, stopPostgres)

-- | Create a temporary database with default configuration, run an action, then clean up.
--
-- This is the recommended way to use ephemeral-pg. The database is
-- guaranteed to be stopped and cleaned up even if an exception is thrown.
--
-- @
-- result <- 'with' $ \\db -> do
--   conn <- Connection.acquire ('connectionSettings' db)
--   -- Use the connection...
-- @
with :: (Database -> IO a) -> IO (Either StartError a)
with = withConfig defaultConfig

-- | Like 'with' but with custom configuration.
--
-- @
-- let config = 'defaultConfig' { databaseName = "testdb" }
-- 'withConfig' config $ \\db -> do
--   -- Use the database...
-- @
withConfig :: Config -> (Database -> IO a) -> IO (Either StartError a)
withConfig config action = mask $ \restore -> runStartup $ do
  db <- liftE $ start config
  liftIO $ do
    a <- restore (action db) `onException` stop db
    stop db
    pure a

-- | Start a temporary database.
--
-- You are responsible for calling 'stop' when done. Prefer 'with' or
-- 'withConfig' when possible.
--
-- @
-- db <- 'start' 'defaultConfig'
-- case db of
--   Right database -> do
--     -- Use database...
--     'stop' database
--   Left err -> handleError err
-- @
start :: Config -> IO (Either StartError Database)
start config = runStartup $ do
  let mTempRoot = getLast config.temporaryRoot

  -- Create data directory
  (dataDir, dataDirIsTemp) <-
    liftE $
      resolveDirectory
        config.dataDirectory
        mTempRoot
        "data"
        createTempDataDirectory

  -- Create socket directory
  (socketDir, socketDirIsTemp) <-
    liftE
      ( resolveDirectory
          config.socketDirectory
          mTempRoot
          "socket"
          createTempSocketDirectory
      )
      `onError` when dataDirIsTemp (removeDirectoryIfExists dataDir)

  -- Get port
  p <-
    liftE (getPort config)
      `onError` cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir

  -- Get username
  username <- liftIO $ getUsername config

  -- Run initdb
  () <-
    liftE (runInitDb config dataDir)
      `onError` cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir

  -- Start postgres
  pgProcess <-
    liftE (startPostgres config dataDir socketDir p username)
      `onError` cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir

  -- Create database
  let dbName = config.databaseName
  () <-
    liftE (runCreateDb config socketDir p username dbName)
      `onError` do
        _ <- stopPostgres pgProcess ShutdownImmediate 5
        cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir

  -- Build cleanup action
  let cleanupAction = do
        when dataDirIsTemp $ do
          -- Use retry to handle pg_stat race
          _ <- retryRemoveDirectory dataDir 5 100000
          pure ()
        when socketDirIsTemp $
          removeDirectoryIfExists socketDir

  pure $
    Database
      { dataDirectory = dataDir,
        socketDirectory = socketDir,
        port = p,
        databaseName = dbName,
        user = username,
        password = config.password,
        process = pgProcess,
        cleanup = cleanupAction,
        dataDirIsTemp = dataDirIsTemp,
        socketDirIsTemp = socketDirIsTemp,
        shutdownMode = resolveShutdownMode config,
        shutdownTimeoutSeconds = resolveShutdownTimeout config
      }
  where
    cleanup :: Bool -> FilePath -> Bool -> FilePath -> IO ()
    cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir = do
      when dataDirIsTemp $ removeDirectoryIfExists dataDir
      when socketDirIsTemp $ removeDirectoryIfExists socketDir

-- | Get port from config or find a free one.
getPort :: Config -> IO (Either StartError Word16)
getPort config = case getLast config.port of
  Just p -> pure $ Right p
  Nothing -> findFreePort

-- | Get username from config or current user.
getUsername :: Config -> IO Text
getUsername config = case config.user of
  "" -> getCurrentUser
  u -> pure u

-- | Resolve shutdown mode from config, falling back to default.
resolveShutdownMode :: Config -> ShutdownMode
resolveShutdownMode config =
  maybe ShutdownGraceful id $ getLast config.shutdownMode

-- | Resolve shutdown timeout from config, falling back to default.
resolveShutdownTimeout :: Config -> Int
resolveShutdownTimeout config =
  maybe defaultShutdownTimeoutSeconds id $ getLast config.shutdownTimeoutSeconds

-- | Stop a database and clean up resources.
--
-- Uses the shutdown mode and timeout from the configuration that was
-- used to start the database.
--
-- Safe to call multiple times (subsequent calls are no-ops).
stop :: Database -> IO ()
stop db = do
  -- Stop postgres using configured shutdown mode and timeout
  _ <- stopPostgres db.process db.shutdownMode db.shutdownTimeoutSeconds

  -- Run cleanup (removes temp directories)
  db.cleanup

-- | Restart a database.
--
-- This stops the postgres server and starts it again, returning a new
-- 'Database' handle with the updated process information.
--
-- The data directory and all database contents are preserved.
-- This is useful for testing scenarios that require a server restart,
-- such as configuration changes that require a restart to take effect.
--
-- @
-- db <- 'start' 'defaultConfig'
-- case db of
--   Right database -> do
--     -- Use database...
--     newDb <- 'restart' database
--     case newDb of
--       Right database' -> -- Use restarted database...
--       Left err -> handleError err
--   Left err -> handleError err
-- @
restart :: Database -> IO (Either StartError Database)
restart db = runStartup $ do
  -- Stop postgres using configured shutdown settings
  liftIO $ do
    _ <- stopPostgres db.process db.shutdownMode db.shutdownTimeoutSeconds
    pure ()

  -- Start postgres again with the same configuration
  newProcess <-
    liftE $
      startPostgres
        defaultConfig
        db.dataDirectory
        db.socketDirectory
        db.port
        db.user

  pure $ db {process = newProcess}

-- | Like 'with' but uses initdb caching for faster startup.
--
-- The first invocation runs initdb and caches the result.
-- Subsequent invocations copy from the cache (using CoW if available).
--
-- @
-- result <- 'withCached' $ \\db -> do
--   conn <- Connection.acquire ('connectionSettings' db)
--   -- Use the connection...
-- @
withCached :: (Database -> IO a) -> IO (Either StartError a)
withCached = withCachedConfig defaultConfig defaultCacheConfig

-- | Like 'withCached' but with custom configuration.
withCachedConfig :: Config -> CacheConfig -> (Database -> IO a) -> IO (Either StartError a)
withCachedConfig config cacheConfig action = mask $ \restore -> runStartup $ do
  db <- liftE $ startCached config cacheConfig
  liftIO $ do
    a <- restore (action db) `onException` stop db
    stop db
    pure a

-- | Start a temporary database using initdb caching.
--
-- If caching is enabled and a cache exists, the data directory is copied
-- from the cache. Otherwise, initdb is run and the result is cached.
startCached :: Config -> CacheConfig -> IO (Either StartError Database)
startCached config cacheConfig
  | not cacheConfig.enabled = start config
  | otherwise = do
      -- Get cache key
      keyResult <- getCacheKey config
      case keyResult of
        Left _err ->
          -- Can't determine cache key, fall back to non-cached start
          start config
        Right cacheKey -> do
          -- Check if cache exists
          cached <- isCached cacheKey cacheConfig.root
          if cached
            then startFromCache config cacheConfig cacheKey
            else startAndCache config cacheConfig cacheKey

-- | Start from an existing cache.
startFromCache :: Config -> CacheConfig -> CacheKey -> IO (Either StartError Database)
startFromCache config cacheConfig cacheKey = runStartup $ do
  let mTempRoot = getLast config.temporaryRoot

  -- Create temporary data directory (to get the path)
  (dataDir, dataDirIsTemp) <- liftE $ createTempDataDirectory mTempRoot

  -- Remove the directory so cp can create it fresh
  -- (otherwise cp -cR creates nested directories on macOS)
  liftIO $ removeDirectoryIfExists dataDir

  -- Restore from cache
  restoreResult <- liftIO $ restoreFromCache cacheKey dataDir cacheConfig.root
  case restoreResult of
    Left _err -> do
      -- Cache restore failed, fall back to non-cached start
      liftIO $ removeDirectoryIfExists dataDir
      liftE $ start config
    Right () -> do
      liftIO $ do
        cleanupRuntimeFiles dataDir
        writePostgresConf config dataDir
      liftE $ continueStartup config dataDir dataDirIsTemp

-- | Start normally and cache the result.
-- Cache is created after initdb but before postgres starts.
startAndCache :: Config -> CacheConfig -> CacheKey -> IO (Either StartError Database)
startAndCache config cacheConfig cacheKey = runStartup $ do
  let mTempRoot = getLast config.temporaryRoot

  -- Create data directory
  (dataDir, dataDirIsTemp) <- liftE $ createTempDataDirectory mTempRoot

  -- Run initdb
  liftE (runInitDb config dataDir)
    `onError` when dataDirIsTemp (removeDirectoryIfExists dataDir)

  -- Cache the data directory NOW (before postgres starts)
  -- This ensures the cache contains only clean initdb output
  liftIO $ when dataDirIsTemp $ do
    _ <- createCache cacheKey dataDir cacheConfig.root
    pure ()

  -- Continue with normal startup from the initialized data directory
  liftE $ continueStartup config dataDir dataDirIsTemp

-- | Continue startup from an existing data directory.
continueStartup :: Config -> FilePath -> Bool -> IO (Either StartError Database)
continueStartup config dataDir dataDirIsTemp = runStartup $ do
  let mTempRoot = getLast config.temporaryRoot

  -- Create socket directory
  (socketDir, socketDirIsTemp) <-
    liftE
      ( resolveDirectory
          config.socketDirectory
          mTempRoot
          "socket"
          createTempSocketDirectory
      )
      `onError` when dataDirIsTemp (removeDirectoryIfExists dataDir)

  -- Get port
  p <-
    liftE (getPort config)
      `onError` cleanupDirs dataDirIsTemp dataDir socketDirIsTemp socketDir

  -- Get username
  username <- liftIO $ getUsername config

  -- Start postgres (initdb already done)
  pgProcess <-
    liftE (startPostgres config dataDir socketDir p username)
      `onError` cleanupDirs dataDirIsTemp dataDir socketDirIsTemp socketDir

  -- Create database
  let dbName = config.databaseName
  () <-
    liftE (runCreateDb config socketDir p username dbName)
      `onError` do
        _ <- stopPostgres pgProcess ShutdownImmediate 5
        cleanupDirs dataDirIsTemp dataDir socketDirIsTemp socketDir

  -- Build cleanup action
  let cleanupAction = do
        when dataDirIsTemp $ do
          _ <- retryRemoveDirectory dataDir 5 100000
          pure ()
        when socketDirIsTemp $
          removeDirectoryIfExists socketDir

  pure $
    Database
      { dataDirectory = dataDir,
        socketDirectory = socketDir,
        port = p,
        databaseName = dbName,
        user = username,
        password = config.password,
        process = pgProcess,
        cleanup = cleanupAction,
        dataDirIsTemp = dataDirIsTemp,
        socketDirIsTemp = socketDirIsTemp,
        shutdownMode = resolveShutdownMode config,
        shutdownTimeoutSeconds = resolveShutdownTimeout config
      }
  where
    cleanupDirs :: Bool -> FilePath -> Bool -> FilePath -> IO ()
    cleanupDirs dataDirIsTemp' dataDir' socketDirIsTemp' socketDir' = do
      when dataDirIsTemp' $ removeDirectoryIfExists dataDir'
      when socketDirIsTemp' $ removeDirectoryIfExists socketDir'
