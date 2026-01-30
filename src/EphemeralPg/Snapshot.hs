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
import EphemeralPg.Process.Postgres (startPostgres, stopPostgres)
import System.Directory (doesDirectoryExist)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- | A snapshot of database state.
data Snapshot = Snapshot
  { -- | Path to the snapshot data directory
    snapshotPath :: FilePath,
    -- | Whether to delete the snapshot when done
    snapshotTemporary :: Bool
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
createSnapshot db = do
  -- Stop postgres gracefully to ensure data is flushed
  stopResult <- stopPostgres (dbProcess db) ShutdownGraceful 30
  case stopResult of
    Just err -> do
      -- Restart postgres and return error
      _ <- restartPostgres db
      pure $ Left $ "Failed to stop postgres for snapshot: " <> T.pack (show err)
    Nothing -> do
      -- Create snapshot directory
      tmpDir <- getCanonicalTemporaryDirectory
      snapshotDir <- createTempDirectory tmpDir "ephpg-snap-"

      -- Detect CoW capability
      cowCapability <- detectCowCapability snapshotDir

      -- Copy data directory to snapshot
      copyResult <- copyDirectory cowCapability (dbDataDirectory db) snapshotDir

      case copyResult of
        Left err -> do
          removeDirectoryIfExists snapshotDir
          _ <- restartPostgres db
          pure $ Left $ "Failed to copy data directory: " <> err
        Right () -> do
          -- Clean up runtime files from the snapshot
          cleanupRuntimeFiles snapshotDir

          -- Restart postgres
          restartResult <- restartPostgres db
          case restartResult of
            Left err -> do
              removeDirectoryIfExists snapshotDir
              pure $ Left err
            Right _newProcess ->
              -- Note: We don't update the Database record with the new process
              -- since Database is immutable. The caller should handle this.
              pure $
                Right $
                  Snapshot
                    { snapshotPath = snapshotDir,
                      snapshotTemporary = True
                    }

-- | Restore a database from a snapshot.
--
-- This stops postgres, replaces the data directory with the snapshot,
-- then restarts postgres. The original database content is lost.
restoreSnapshot :: Snapshot -> Database -> IO (Either Text ())
restoreSnapshot snapshot db = do
  -- Verify snapshot exists
  exists <- doesDirectoryExist (snapshotPath snapshot)
  if not exists
    then pure $ Left $ "Snapshot not found: " <> T.pack (snapshotPath snapshot)
    else do
      -- Stop postgres
      stopResult <- stopPostgres (dbProcess db) ShutdownGraceful 30
      case stopResult of
        Just err -> do
          _ <- restartPostgres db
          pure $ Left $ "Failed to stop postgres for restore: " <> T.pack (show err)
        Nothing -> do
          -- Remove current data directory
          removeDirectoryIfExists (dbDataDirectory db)

          -- Detect CoW capability
          cowCapability <- detectCowCapability (snapshotPath snapshot)

          -- Copy snapshot to data directory
          copyResult <- copyDirectory cowCapability (snapshotPath snapshot) (dbDataDirectory db)

          case copyResult of
            Left err -> do
              -- This is bad - data directory is in inconsistent state
              pure $ Left $ "Failed to restore snapshot, database may be corrupted: " <> err
            Right () -> do
              -- Restart postgres
              restartResult <- restartPostgres db
              case restartResult of
                Left err -> pure $ Left err
                Right _newProcess -> pure $ Right ()

-- | Delete a snapshot.
--
-- Only deletes temporary snapshots. Permanent snapshots are left alone.
deleteSnapshot :: Snapshot -> IO ()
deleteSnapshot snapshot =
  if snapshotTemporary snapshot
    then removeDirectoryIfExists (snapshotPath snapshot)
    else pure ()

-- | Restart postgres after a snapshot/restore operation.
restartPostgres :: Database -> IO (Either Text PostgresProcess)
restartPostgres db = do
  result <-
    startPostgres
      defaultConfig
      (dbDataDirectory db)
      (dbSocketDirectory db)
      (dbPort db)
      (dbUser db)
  case result of
    Left err -> pure $ Left $ "Failed to restart postgres: " <> T.pack (show err)
    Right newProcess -> pure $ Right newProcess
