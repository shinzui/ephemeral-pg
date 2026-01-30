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
--   let config = Pg.'defaultConfig' { Pg.configDatabaseName = "testdb" }
--   Pg.'withConfig' config $ \\db -> do
--     -- Use the database...
--     pure ()
-- @
module EphemeralPg
  ( -- * Database Handle
    Database,
    connectionSettings,
    connectionString,
    dataDirectory,
    socketDirectory,
    port,
    databaseName,
    user,

    -- * Lifecycle Management
    with,
    withConfig,
    start,
    stop,

    -- * Configuration
    Config (..),
    DirectoryConfig (..),
    ShutdownMode (..),
    defaultConfig,
    verboseConfig,
    autoExplainConfig,

    -- * Errors
    StartError (..),
    StopError (..),
    renderStartError,
    renderStopError,
  )
where

import Control.Exception (mask, onException)
import Control.Monad (when)
import Data.Monoid (Last (..))
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
    dataDirectory,
    databaseName,
    port,
    socketDirectory,
    user,
  )
import EphemeralPg.Error
  ( StartError (..),
    StopError (..),
    renderStartError,
    renderStopError,
  )
import EphemeralPg.Internal.Directory
  ( createTempDataDirectory,
    createTempSocketDirectory,
    removeDirectoryIfExists,
    resolveDirectory,
    retryRemoveDirectory,
  )
import EphemeralPg.Internal.Port (findFreePort)
import EphemeralPg.Process (getCurrentUser)
import EphemeralPg.Process.CreateDb (runCreateDb)
import EphemeralPg.Process.InitDb (runInitDb)
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
-- let config = 'defaultConfig' { configDatabaseName = "testdb" }
-- 'withConfig' config $ \\db -> do
--   -- Use the database...
-- @
withConfig :: Config -> (Database -> IO a) -> IO (Either StartError a)
withConfig config action = mask $ \restore -> do
  result <- start config
  case result of
    Left err -> pure (Left err)
    Right db -> do
      a <- restore (action db) `onException` stop db
      stop db
      pure (Right a)

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
start config = do
  -- Resolve temp root
  let mTempRoot = getLast (configTemporaryRoot config)

  -- Create data directory
  dataResult <-
    resolveDirectory
      (configDataDirectory config)
      mTempRoot
      "data"
      createTempDataDirectory

  case dataResult of
    Left err -> pure $ Left err
    Right (dataDir, dataDirIsTemp) -> do
      -- Create socket directory
      socketResult <-
        resolveDirectory
          (configSocketDirectory config)
          mTempRoot
          "socket"
          createTempSocketDirectory

      case socketResult of
        Left err -> do
          -- Clean up data directory if we created it
          when dataDirIsTemp $ removeDirectoryIfExists dataDir
          pure $ Left err
        Right (socketDir, socketDirIsTemp) -> do
          -- Get port
          portResult <- case getLast (configPort config) of
            Just p -> pure $ Right p
            Nothing -> findFreePort

          case portResult of
            Left err -> do
              cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir
              pure $ Left err
            Right p -> do
              -- Get username
              username <- case configUser config of
                "" -> getCurrentUser
                u -> pure u

              -- Run initdb
              initResult <- runInitDb config dataDir

              case initResult of
                Left err -> do
                  cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir
                  pure $ Left err
                Right () -> do
                  -- Start postgres
                  pgResult <- startPostgres config dataDir socketDir p username

                  case pgResult of
                    Left err -> do
                      cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir
                      pure $ Left err
                    Right pgProcess -> do
                      -- Create database
                      let dbName = configDatabaseName config
                      createResult <- runCreateDb config socketDir p username dbName

                      case createResult of
                        Left err -> do
                          _ <- stopPostgres pgProcess ShutdownImmediate 5
                          cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir
                          pure $ Left err
                        Right () -> do
                          -- Build cleanup action
                          let cleanupAction = do
                                when dataDirIsTemp $ do
                                  -- Use retry to handle pg_stat race
                                  _ <- retryRemoveDirectory dataDir 5 100000
                                  pure ()
                                when socketDirIsTemp $
                                  removeDirectoryIfExists socketDir

                          pure $
                            Right $
                              Database
                                { dbDataDirectory = dataDir,
                                  dbSocketDirectory = socketDir,
                                  dbPort = p,
                                  dbDatabaseName = dbName,
                                  dbUser = username,
                                  dbPassword = configPassword config,
                                  dbProcess = pgProcess,
                                  dbCleanup = cleanupAction,
                                  dbDataDirIsTemp = dataDirIsTemp,
                                  dbSocketDirIsTemp = socketDirIsTemp
                                }
  where
    cleanup :: Bool -> FilePath -> Bool -> FilePath -> IO ()
    cleanup dataDirIsTemp dataDir socketDirIsTemp socketDir = do
      when dataDirIsTemp $ removeDirectoryIfExists dataDir
      when socketDirIsTemp $ removeDirectoryIfExists socketDir

-- | Stop a database and clean up resources.
--
-- This sends SIGTERM to postgres and waits for graceful shutdown,
-- then removes temporary directories.
--
-- Safe to call multiple times (subsequent calls are no-ops).
stop :: Database -> IO ()
stop db = do
  let mode = ShutdownGraceful
  let timeoutSecs = defaultShutdownTimeoutSeconds

  -- Stop postgres
  _ <- stopPostgres (dbProcess db) mode timeoutSecs

  -- Run cleanup (removes temp directories)
  dbCleanup db
