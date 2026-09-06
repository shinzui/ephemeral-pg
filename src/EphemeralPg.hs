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
    sweepStaleInstances,
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
    CacheKey (..),
    CowCapability (..),
    CowMethod (..),
    defaultCacheConfig,
    getCacheKey,
    clearCache,
    clearAllCaches,

    -- * Errors
    StartError (..),
    StopError (..),
    renderStartError,
    renderStopError,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, finally, mask, onException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
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
  ( ResourceError (..),
    StartError (..),
    StopError (..),
    renderStartError,
    renderStopError,
  )
import EphemeralPg.Internal.Cache
  ( CacheConfig (..),
    CacheKey (..),
    cleanupRuntimeFiles,
    clearAllCaches,
    clearCache,
    createCache,
    defaultCacheConfig,
    getCacheKey,
    isCached,
    restoreFromCache,
  )
import EphemeralPg.Internal.CopyOnWrite
  ( CowCapability (..),
    CowMethod (..),
  )
import EphemeralPg.Internal.Directory
  ( createTempDataDirectory,
    createTempSocketDirectory,
    removeDirectoryIfExists,
    resolveDirectory,
    retryRemoveDirectory,
  )
import EphemeralPg.Internal.Except (liftE, runStartup)
import EphemeralPg.Internal.Instance (registerInstance, releaseInstance, safeDirectory)
import EphemeralPg.Internal.Port (findFreePort)
import EphemeralPg.Internal.ProcessIdentity (systemInspector)
import EphemeralPg.Internal.Sweep qualified as Sweep
import EphemeralPg.Process (getCurrentUser)
import EphemeralPg.Process.CreateDb (runCreateDb)
import EphemeralPg.Process.InitDb (runInitDb, writePostgresConf)
import EphemeralPg.Process.Postgres (startPostgres, stopPostgres)
import System.Directory qualified as D

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
start config = startManaged config Nothing

-- | Stop provably abandoned PostgreSQL servers and return the canonical paths
-- of temporary data directories removed, in sorted order. Uses 'temporaryRoot'
-- (or the system temporary directory), independently of 'sweepStaleOnStart'.
-- Live ownership locks, permanent data, sockets, snapshots and caches are
-- excluded. Fast shutdown waits up to five seconds per server; uncertain or
-- unresponsive instances are retained. Cleanup after SIGKILL is delayed until
-- the next sweep. Requires local filesystem locks and inspectable processes.
sweepStaleInstances :: Config -> IO [FilePath]
sweepStaleInstances = Sweep.sweepStaleInstances

-- Allocation, startup and cleanup share one ownership transfer. A cache fallback
-- reuses the protected allocation, so it cannot introduce a second sweep.
startManaged :: Config -> Maybe CacheConfig -> IO (Either StartError Database)
startManaged config cache = mask $ \restore -> do
  when (maybe True id $ getLast config.sweepStaleOnStart) $
    restore (sweepStaleInstances config) >> pure ()
  resources <- newIORef (pure ())
  let clean = readIORef resources >>= id
  result <-
    ( runStartup $ do
        root <- liftIO $ maybe D.getTemporaryDirectory pure (getLast config.temporaryRoot) >>= D.canonicalizePath
        (dataDir, isTemp, release) <- liftE $ case config.dataDirectory of
          DirectoryPermanent _ ->
            fmap (fmap (\(path, temp) -> (path, temp, pure ()))) $
              resolveDirectory config.dataDirectory (Just root) "data" createTempDataDirectory
          DirectoryTemporary -> do
            acquired <- try @IOException $ registerInstance root
            pure $ case acquired of
              Left err -> Left $ ResourceError $ DirectoryCreationFailed root (T.pack $ show err)
              Right (path, lease) -> Right (path, True, releaseInstance lease)
        cleaned <- liftIO $ newIORef False
        let cleanData = do
              already <- atomicModifyIORef' cleaned (\old -> (True, old))
              unless already $
                ( do
                    _ <- try @IOException $ when isTemp $ do
                      -- Snapshot operations can replace the process behind the
                      -- exported immutable handle. Never delete an active cluster.
                      let awaitUnused attempts = do
                            unused <- Sweep.directoryUnused systemInspector dataDir
                            if unused || attempts == (0 :: Int)
                              then pure unused
                              else threadDelay 50000 >> awaitUnused (attempts - 1)
                      unused <- awaitUnused 3
                      when unused $ do
                        _ <- safeDirectory dataDir
                        canonical <- D.canonicalizePath dataDir
                        when (canonical == dataDir) $ do
                          _ <- retryRemoveDirectory dataDir 5 100000
                          pure ()
                    pure ()
                )
                  `finally` release
        liftIO $ writeIORef resources cleanData
        (socketDir, socketIsTemp) <-
          liftE $
            resolveDirectory config.socketDirectory (Just root) "socket" createTempSocketDirectory
        let cleanDirs = cleanData `finally` when socketIsTemp (removeDirectoryIfExists socketDir)
        liftIO $ writeIORef resources cleanDirs
        p <- liftE $ restore $ getPort config
        username <- liftIO $ restore $ getUsername config
        liftE $ restore $ initialize config cache dataDir isTemp
        -- startPostgres masks creation and cleans up cancellation during readiness.
        pgProcess <- liftE $ startPostgres config dataDir socketDir p username
        let abort = do
              outcome <- stopPostgres pgProcess ShutdownImmediate 5
              case outcome of
                Nothing -> cleanDirs
                Just _ -> release -- Keep uncertain data for a later sweep.
        liftIO $ writeIORef resources abort
        liftE $ restore $ runCreateDb config socketDir p username config.databaseName
        pure
          Database
            { dataDirectory = dataDir,
              socketDirectory = socketDir,
              port = p,
              databaseName = config.databaseName,
              user = username,
              password = config.password,
              process = pgProcess,
              cleanup = cleanDirs,
              dataDirIsTemp = isTemp,
              socketDirIsTemp = socketIsTemp,
              shutdownMode = resolveShutdownMode config,
              shutdownTimeoutSeconds = resolveShutdownTimeout config
            }
    )
      `onException` clean
  case result of
    Left _ -> clean >> pure result
    Right _ -> pure result

initialize :: Config -> Maybe CacheConfig -> FilePath -> Bool -> IO (Either StartError ())
initialize config cache dataDir isTemp = case cache of
  Just cacheConfig | cacheConfig.enabled && isTemp -> do
    keyResult <- getCacheKey config
    case keyResult of
      Left _ -> runInitDb config dataDir
      Right key -> do
        cached <- isCached key cacheConfig.root
        if cached
          then do
            D.removeDirectory dataDir
            restored <- restoreFromCache key dataDir cacheConfig.root
            case restored of
              Right () -> cleanupRuntimeFiles dataDir >> writePostgresConf config dataDir >> pure (Right ())
              Left _ -> do
                removeDirectoryIfExists dataDir
                D.createDirectory dataDir
                runInitDb config dataDir
          else do
            initialized <- runInitDb config dataDir
            case initialized of
              Left err -> pure $ Left err
              Right () -> createCache key dataDir cacheConfig.root >> pure (Right ())
  _ -> runInitDb config dataDir

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
  outcome <- stopPostgres db.process db.shutdownMode db.shutdownTimeoutSeconds
  case outcome of
    Nothing -> db.cleanup
    Just _ -> pure () -- Preserve ownership and data when shutdown is uncertain.

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

-- | Start with a reusable initialization cache. Permanent data directories use
-- ordinary initialization and are never registered for stale cleanup.
startCached :: Config -> CacheConfig -> IO (Either StartError Database)
startCached config cacheConfig = startManaged config (Just cacheConfig)
