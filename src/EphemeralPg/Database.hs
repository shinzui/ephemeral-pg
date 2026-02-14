-- | Database handle and connection utilities.
--
-- This module provides the 'Database' type representing a running
-- PostgreSQL instance, and functions to extract connection information.
module EphemeralPg.Database
  ( -- * Database handle
    Database (..),
    PostgresProcess (..),

    -- * Connection information
    connectionSettings,
    connectionString,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Hasql.Connection.Settings qualified as Settings
import System.Posix.Types (CPid)
import System.Process.Typed (Process)

-- | Handle to a running PostgreSQL server process.
data PostgresProcess = PostgresProcess
  { -- | The typed-process handle
    process :: Process () () (),
    -- | Process ID for signal handling
    pid :: CPid
  }

instance Show PostgresProcess where
  show pp =
    "PostgresProcess { pid = " <> show pp.pid <> " }"

-- | A running PostgreSQL database instance.
--
-- Obtain via 'EphemeralPg.start' or 'EphemeralPg.with'. The database is
-- ready to accept connections immediately after creation.
data Database = Database
  { -- | Path to the PostgreSQL data directory
    dataDirectory :: FilePath,
    -- | Path to the Unix socket directory
    socketDirectory :: FilePath,
    -- | Port number the server is listening on
    port :: Word16,
    -- | Name of the database
    databaseName :: Text,
    -- | Username for connections
    user :: Text,
    -- | Optional password
    password :: Maybe Text,
    -- | Handle to the running postgres process
    process :: PostgresProcess,
    -- | Cleanup action (removes temp directories if needed)
    cleanup :: IO (),
    -- | Whether data directory is temporary
    dataDirIsTemp :: Bool,
    -- | Whether socket directory is temporary
    socketDirIsTemp :: Bool
  }

instance Show Database where
  show db =
    "Database { port = "
      <> show db.port
      <> ", database = "
      <> show db.databaseName
      <> ", user = "
      <> show db.user
      <> ", dataDir = "
      <> show db.dataDirectory
      <> " }"

-- | Get hasql connection settings for this database.
--
-- Example:
--
-- @
-- db <- 'EphemeralPg.start' 'EphemeralPg.defaultConfig'
-- case db of
--   Right database -> do
--     Right conn <- Connection.acquire ('connectionSettings' database)
--     -- use connection...
--   Left err -> handleError err
-- @
connectionSettings :: Database -> Settings.Settings
connectionSettings db =
  mconcat
    [ Settings.hostAndPort (T.pack db.socketDirectory) db.port,
      Settings.dbname db.databaseName,
      Settings.user db.user,
      maybe mempty Settings.password db.password
    ]

-- | Get a libpq-compatible connection string.
--
-- Format: @host=/path/to/socket port=5432 dbname=postgres user=username@
--
-- Useful for interoperating with other PostgreSQL libraries.
connectionString :: Database -> Text
connectionString db =
  T.unwords
    [ "host=" <> T.pack db.socketDirectory,
      "port=" <> T.pack (show db.port),
      "dbname=" <> db.databaseName,
      "user=" <> db.user
    ]
