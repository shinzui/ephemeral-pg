-- | Database snapshot functionality for test isolation.
--
-- This module provides snapshot and restore capabilities for ephemeral
-- PostgreSQL databases. Snapshots capture the entire database state
-- and can be restored to reset the database between tests.
--
-- = Example Usage
--
-- @
-- result <- Pg.'with' $ \\db -> do
--   -- Initial setup
--   runMigrations db
--
--   -- Take a snapshot after setup
--   snapshot <- Pg.'createSnapshot' db
--
--   -- Run test 1
--   insertTestData db
--   runTest1 db
--
--   -- Restore to clean state
--   Pg.'restoreSnapshot' snapshot db
--
--   -- Run test 2 with clean state
--   insertTestData db
--   runTest2 db
-- @
module EphemeralPg.Snapshot
  ( -- * Snapshot Types
    Snapshot (..),

    -- * Snapshot Operations
    createSnapshot,
    restoreSnapshot,
    deleteSnapshot,
  )
where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import Data.Text (Text)
import Data.Text qualified as T
import EphemeralPg.Config (ShutdownMode (..), defaultConfig)
import EphemeralPg.Database (Database (..), PostgresProcess (..))
import EphemeralPg.Internal.Cache (cleanupRuntimeFiles)
import EphemeralPg.Internal.CopyOnWrite
  ( copyDirectory,
    detectCowCapability,
  )
import EphemeralPg.Internal.Directory (removeDirectoryIfExists)
import EphemeralPg.Internal.Except (onError)
import EphemeralPg.Process.Postgres (startPostgres, stopPostgres)
import System.Directory (doesDirectoryExist)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- | A snapshot of database state.
data Snapshot = Snapshot
  { -- | Path to the snapshot data directory
    path :: FilePath,
    -- | Whether to delete the snapshot when done
    temporary :: Bool
  }
  deriving stock (Eq, Show)

-- | Create a snapshot of the current database state.
--
-- This stops postgres, copies the data directory using copy-on-write
-- if available, then restarts postgres.
--
-- Note: This is a relatively expensive operation and should be used
-- sparingly (e.g., once after setup, not between every test).
createSnapshot :: Database -> IO (Either Text Snapshot)
createSnapshot db = runExceptT $ do
  stopPostgresE "snapshot" db
    `onError` void (restartPostgres db)
  snapshotDir <- liftIO $ do
    tmpDir <- getCanonicalTemporaryDirectory
    createTempDirectory tmpDir "ephpg-snap-"
  cowCapability <- liftIO $ detectCowCapability snapshotDir
  withExceptT
    ("Failed to copy data directory: " <>)
    (ExceptT $ copyDirectory cowCapability db.dataDirectory snapshotDir)
    `onError` do
      removeDirectoryIfExists snapshotDir
      void $ restartPostgres db
  liftIO $ cleanupRuntimeFiles snapshotDir
  -- Note: We don't update the Database record with the new process
  -- since Database is immutable. The caller should handle this.
  _ <-
    ExceptT (restartPostgres db)
      `onError` removeDirectoryIfExists snapshotDir
  pure
    Snapshot
      { path = snapshotDir,
        temporary = True
      }

-- | Restore a database from a snapshot.
--
-- This stops postgres, replaces the data directory with the snapshot,
-- then restarts postgres. The original database content is lost.
restoreSnapshot :: Snapshot -> Database -> IO (Either Text ())
restoreSnapshot snapshot db = runExceptT $ do
  exists <- liftIO $ doesDirectoryExist snapshot.path
  unless exists $
    throwE $
      "Snapshot not found: " <> T.pack snapshot.path
  stopPostgresE "restore" db
    `onError` void (restartPostgres db)
  liftIO $ removeDirectoryIfExists db.dataDirectory
  cowCapability <- liftIO $ detectCowCapability snapshot.path
  withExceptT
    ("Failed to restore snapshot, database may be corrupted: " <>)
    (ExceptT $ copyDirectory cowCapability snapshot.path db.dataDirectory)
  _ <- ExceptT $ restartPostgres db
  pure ()

-- | Delete a snapshot.
--
-- Only deletes temporary snapshots. Permanent snapshots are left alone.
deleteSnapshot :: Snapshot -> IO ()
deleteSnapshot snapshot =
  if snapshot.temporary
    then removeDirectoryIfExists snapshot.path
    else pure ()

-- | Stop postgres, failing with a descriptive error.
stopPostgresE :: Text -> Database -> ExceptT Text IO ()
stopPostgresE purpose db = do
  stopResult <- liftIO $ stopPostgres db.process ShutdownGraceful 30
  case stopResult of
    Just err ->
      throwE $ "Failed to stop postgres for " <> purpose <> ": " <> T.pack (show err)
    Nothing -> pure ()

-- | Restart postgres after a snapshot/restore operation.
restartPostgres :: Database -> IO (Either Text PostgresProcess)
restartPostgres db = do
  result <-
    startPostgres
      defaultConfig
      db.dataDirectory
      db.socketDirectory
      db.port
      db.user
  case result of
    Left err -> pure $ Left $ "Failed to restart postgres: " <> T.pack (show err)
    Right newProcess -> pure $ Right newProcess
