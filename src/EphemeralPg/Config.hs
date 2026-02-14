-- | Configuration for temporary PostgreSQL databases.
--
-- This module provides the 'Config' type and predefined configurations
-- for common use cases.
--
-- Configurations can be combined using the 'Semigroup' instance:
--
-- @
-- myConfig = 'defaultConfig' <> mempty { databaseName = "testdb" }
-- @
module EphemeralPg.Config
  ( -- * Configuration type
    Config (..),
    DirectoryConfig (..),
    ShutdownMode (..),

    -- * Predefined configurations
    defaultConfig,
    verboseConfig,
    autoExplainConfig,

    -- * Default values
    defaultPostgresSettings,
    defaultInitDbArgs,
    defaultConnectionTimeoutSeconds,
    defaultShutdownTimeoutSeconds,

    -- * Socket path limits
    maxSocketPathLength,
  )
where

import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import System.IO (Handle)

-- | Configuration for a directory.
data DirectoryConfig
  = -- | Create a temporary directory that is deleted on cleanup (default)
    DirectoryTemporary
  | -- | Use a specific path that is NOT deleted on cleanup
    DirectoryPermanent FilePath
  deriving stock (Eq, Show)

-- | How to shut down the PostgreSQL server.
data ShutdownMode
  = -- | Send SIGTERM, wait for clean shutdown (default)
    ShutdownGraceful
  | -- | Send SIGINT for fast shutdown (rolls back active transactions)
    ShutdownFast
  | -- | Send SIGQUIT for immediate shutdown
    ShutdownImmediate
  deriving stock (Eq, Show)

-- | Configuration for a temporary PostgreSQL database.
--
-- Combine configurations using '<>'. Later values take precedence
-- for 'Last' fields.
data Config = Config
  { -- | Port to listen on. 'Nothing' means auto-select a free port.
    port :: Last Word16,
    -- | Database name to create.
    databaseName :: Text,
    -- | Username for the database.
    user :: Text,
    -- | Optional password.
    password :: Maybe Text,
    -- | Data directory configuration.
    dataDirectory :: DirectoryConfig,
    -- | Socket directory configuration.
    socketDirectory :: DirectoryConfig,
    -- | Root directory for temporary files.
    temporaryRoot :: Last FilePath,
    -- | postgresql.conf settings.
    postgresSettings :: [(Text, Text)],
    -- | Additional arguments for initdb.
    initDbArgs :: [Text],
    -- | Additional arguments for postgres server.
    postgresArgs :: [Text],
    -- | Additional arguments for createdb.
    createDbArgs :: [Text],
    -- | Timeout waiting for postgres to accept connections (seconds).
    connectionTimeoutSeconds :: Last Int,
    -- | Timeout waiting for graceful shutdown (seconds).
    shutdownTimeoutSeconds :: Last Int,
    -- | How to shut down postgres.
    shutdownMode :: Last ShutdownMode,
    -- | Handle for postgres stdout (Nothing = discard).
    stdout :: Last (Maybe Handle),
    -- | Handle for postgres stderr (Nothing = discard).
    stderr :: Last (Maybe Handle)
  }
  deriving stock (Show)

instance Semigroup Config where
  a <> b =
    Config
      { port = a.port <> b.port,
        databaseName =
          if b.databaseName == ""
            then a.databaseName
            else b.databaseName,
        user =
          if b.user == ""
            then a.user
            else b.user,
        password = b.password <|> a.password,
        dataDirectory = combineDir a.dataDirectory b.dataDirectory,
        socketDirectory = combineDir a.socketDirectory b.socketDirectory,
        temporaryRoot = a.temporaryRoot <> b.temporaryRoot,
        postgresSettings = a.postgresSettings <> b.postgresSettings,
        initDbArgs = a.initDbArgs <> b.initDbArgs,
        postgresArgs = a.postgresArgs <> b.postgresArgs,
        createDbArgs = a.createDbArgs <> b.createDbArgs,
        connectionTimeoutSeconds = a.connectionTimeoutSeconds <> b.connectionTimeoutSeconds,
        shutdownTimeoutSeconds = a.shutdownTimeoutSeconds <> b.shutdownTimeoutSeconds,
        shutdownMode = a.shutdownMode <> b.shutdownMode,
        stdout = a.stdout <> b.stdout,
        stderr = a.stderr <> b.stderr
      }
    where
      combineDir DirectoryTemporary d = d
      combineDir d DirectoryTemporary = d
      combineDir _ d = d

      (<|>) :: Maybe a -> Maybe a -> Maybe a
      (<|>) Nothing x = x
      (<|>) x _ = x

instance Monoid Config where
  mempty =
    Config
      { port = Last Nothing,
        databaseName = "",
        user = "",
        password = Nothing,
        dataDirectory = DirectoryTemporary,
        socketDirectory = DirectoryTemporary,
        temporaryRoot = Last Nothing,
        postgresSettings = [],
        initDbArgs = [],
        postgresArgs = [],
        createDbArgs = [],
        connectionTimeoutSeconds = Last Nothing,
        shutdownTimeoutSeconds = Last Nothing,
        shutdownMode = Last Nothing,
        stdout = Last Nothing,
        stderr = Last Nothing
      }

-- | Maximum socket path length for Unix domain sockets.
--
-- This is 104 bytes on macOS and 108 bytes on Linux.
-- We use the more conservative macOS limit.
maxSocketPathLength :: Int
maxSocketPathLength = 104

-- | Default connection timeout in seconds.
defaultConnectionTimeoutSeconds :: Int
defaultConnectionTimeoutSeconds = 60

-- | Default shutdown timeout in seconds.
defaultShutdownTimeoutSeconds :: Int
defaultShutdownTimeoutSeconds = 30

-- | Default PostgreSQL settings optimized for testing.
--
-- These settings disable durability features for maximum performance
-- and are NOT suitable for production use.
defaultPostgresSettings :: [(Text, Text)]
defaultPostgresSettings =
  [ ("shared_buffers", "'12MB'"),
    ("fsync", "'off'"),
    ("synchronous_commit", "'off'"),
    ("full_page_writes", "'off'"),
    ("random_page_cost", "'1.0'"),
    ("log_min_messages", "'PANIC'"),
    ("log_min_error_statement", "'PANIC'"),
    ("log_statement", "'none'"),
    ("client_min_messages", "'ERROR'"),
    ("wal_level", "'minimal'"),
    ("max_wal_senders", "0"),
    ("archive_mode", "'off'"),
    -- Listen on localhost for IPv4
    ("listen_addresses", "'127.0.0.1'")
  ]

-- | Default initdb arguments.
defaultInitDbArgs :: [Text]
defaultInitDbArgs =
  [ "--no-sync",
    "--encoding=UTF8",
    "--no-locale",
    "--auth=trust"
  ]

-- | High-performance configuration suitable for testing.
--
-- This configuration:
--
-- * Auto-selects a free port
-- * Uses temporary directories (auto-cleaned)
-- * Disables durability features for speed
-- * Uses the current system user
-- * Creates a database named "postgres"
defaultConfig :: Config
defaultConfig =
  Config
    { port = Last Nothing,
      databaseName = "postgres",
      user = "", -- Will use current user
      password = Nothing,
      dataDirectory = DirectoryTemporary,
      socketDirectory = DirectoryTemporary,
      temporaryRoot = Last Nothing,
      postgresSettings = defaultPostgresSettings,
      initDbArgs = defaultInitDbArgs,
      postgresArgs = [],
      createDbArgs = [],
      connectionTimeoutSeconds = Last (Just defaultConnectionTimeoutSeconds),
      shutdownTimeoutSeconds = Last (Just defaultShutdownTimeoutSeconds),
      shutdownMode = Last (Just ShutdownGraceful),
      stdout = Last (Just Nothing), -- Discard by default
      stderr = Last (Just Nothing) -- Discard by default
    }

-- | Configuration with verbose PostgreSQL logging.
--
-- Combine with 'defaultConfig':
--
-- @
-- myConfig = 'defaultConfig' <> 'verboseConfig'
-- @
verboseConfig :: Config
verboseConfig =
  mempty
    { postgresSettings =
        [ ("log_min_duration_statement", "'0'"),
          ("log_min_messages", "'DEBUG1'"),
          ("log_min_error_statement", "'DEBUG1'"),
          ("log_checkpoints", "'on'"),
          ("log_connections", "'on'"),
          ("log_disconnections", "'on'"),
          ("log_lock_waits", "'on'"),
          ("log_temp_files", "'0'"),
          ("log_autovacuum_min_duration", "'0'"),
          ("log_line_prefix", "'%t [%p]: '")
        ]
    }

-- | Configuration that enables auto_explain for query analysis.
--
-- Logs query plans for queries exceeding the specified duration.
--
-- @
-- myConfig = 'defaultConfig' <> 'autoExplainConfig' 100  -- Log queries > 100ms
-- @
autoExplainConfig :: Int -> Config
autoExplainConfig minDurationMs =
  mempty
    { postgresSettings =
        [ ("shared_preload_libraries", "'auto_explain'"),
          ("auto_explain.log_min_duration", "'" <> ms <> "'"),
          ("auto_explain.log_analyze", "'on'")
        ]
    }
  where
    ms :: Text
    ms = T.pack (show minDurationMs) <> "ms"
