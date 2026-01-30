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

    -- * Accessors
    dataDirectory,
    socketDirectory,
    port,
    databaseName,
    user,
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
    postgresProcess :: Process () () (),
    -- | Process ID for signal handling
    postgresPid :: CPid
  }

instance Show PostgresProcess where
  show PostgresProcess {postgresPid} =
    "PostgresProcess { pid = " <> show postgresPid <> " }"

-- | A running PostgreSQL database instance.
--
-- Obtain via 'EphemeralPg.start' or 'EphemeralPg.with'. The database is
-- ready to accept connections immediately after creation.
data Database = Database
  { -- | Path to the PostgreSQL data directory
    dbDataDirectory :: FilePath,
    -- | Path to the Unix socket directory
    dbSocketDirectory :: FilePath,
    -- | Port number the server is listening on
    dbPort :: Word16,
    -- | Name of the database
    dbDatabaseName :: Text,
    -- | Username for connections
    dbUser :: Text,
    -- | Optional password
    dbPassword :: Maybe Text,
    -- | Handle to the running postgres process
    dbProcess :: PostgresProcess,
    -- | Cleanup action (removes temp directories if needed)
    dbCleanup :: IO (),
    -- | Whether data directory is temporary
    dbDataDirIsTemp :: Bool,
    -- | Whether socket directory is temporary
    dbSocketDirIsTemp :: Bool
  }

instance Show Database where
  show db =
    "Database { port = "
      <> show (dbPort db)
      <> ", database = "
      <> show (dbDatabaseName db)
      <> ", user = "
      <> show (dbUser db)
      <> ", dataDir = "
      <> show (dbDataDirectory db)
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
    [ Settings.hostAndPort (T.pack $ dbSocketDirectory db) (dbPort db),
      Settings.dbname (dbDatabaseName db),
      Settings.user (dbUser db),
      maybe mempty Settings.password (dbPassword db)
    ]

-- | Get a libpq-compatible connection string.
--
-- Format: @host=/path/to/socket port=5432 dbname=postgres user=username@
--
-- Useful for interoperating with other PostgreSQL libraries.
connectionString :: Database -> Text
connectionString db =
  T.unwords
    [ "host=" <> T.pack (dbSocketDirectory db),
      "port=" <> T.pack (show $ dbPort db),
      "dbname=" <> dbDatabaseName db,
      "user=" <> dbUser db
    ]

-- | Get the data directory path.
dataDirectory :: Database -> FilePath
dataDirectory = dbDataDirectory

-- | Get the socket directory path.
socketDirectory :: Database -> FilePath
socketDirectory = dbSocketDirectory

-- | Get the port number.
port :: Database -> Word16
port = dbPort

-- | Get the database name.
databaseName :: Database -> Text
databaseName = dbDatabaseName

-- | Get the username.
user :: Database -> Text
user = dbUser
