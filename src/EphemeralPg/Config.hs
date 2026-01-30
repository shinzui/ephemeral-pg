-- | Configuration for temporary PostgreSQL databases.
--
-- This module provides the 'Config' type and predefined configurations
-- for common use cases.
--
-- Configurations can be combined using the 'Semigroup' instance:
--
-- @
-- myConfig = 'defaultConfig' <> mempty { configDatabaseName = "testdb" }
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
    configPort :: Last Word16,
    -- | Database name to create.
    configDatabaseName :: Text,
    -- | Username for the database.
    configUser :: Text,
    -- | Optional password.
    configPassword :: Maybe Text,
    -- | Data directory configuration.
    configDataDirectory :: DirectoryConfig,
    -- | Socket directory configuration.
    configSocketDirectory :: DirectoryConfig,
    -- | Root directory for temporary files.
    configTemporaryRoot :: Last FilePath,
    -- | postgresql.conf settings.
    configPostgresSettings :: [(Text, Text)],
    -- | Additional arguments for initdb.
    configInitDbArgs :: [Text],
    -- | Additional arguments for postgres server.
    configPostgresArgs :: [Text],
    -- | Additional arguments for createdb.
    configCreateDbArgs :: [Text],
    -- | Timeout waiting for postgres to accept connections (seconds).
    configConnectionTimeoutSeconds :: Last Int,
    -- | Timeout waiting for graceful shutdown (seconds).
    configShutdownTimeoutSeconds :: Last Int,
    -- | How to shut down postgres.
    configShutdownMode :: Last ShutdownMode,
    -- | Handle for postgres stdout (Nothing = discard).
    configStdout :: Last (Maybe Handle),
    -- | Handle for postgres stderr (Nothing = discard).
    configStderr :: Last (Maybe Handle)
  }
  deriving stock (Show)

instance Semigroup Config where
  a <> b =
    Config
      { configPort = configPort a <> configPort b,
        configDatabaseName =
          if configDatabaseName b == ""
            then configDatabaseName a
            else configDatabaseName b,
        configUser =
          if configUser b == ""
            then configUser a
            else configUser b,
        configPassword = configPassword b <|> configPassword a,
        configDataDirectory = combineDir (configDataDirectory a) (configDataDirectory b),
        configSocketDirectory = combineDir (configSocketDirectory a) (configSocketDirectory b),
        configTemporaryRoot = configTemporaryRoot a <> configTemporaryRoot b,
        configPostgresSettings = configPostgresSettings a <> configPostgresSettings b,
        configInitDbArgs = configInitDbArgs a <> configInitDbArgs b,
        configPostgresArgs = configPostgresArgs a <> configPostgresArgs b,
        configCreateDbArgs = configCreateDbArgs a <> configCreateDbArgs b,
        configConnectionTimeoutSeconds = configConnectionTimeoutSeconds a <> configConnectionTimeoutSeconds b,
        configShutdownTimeoutSeconds = configShutdownTimeoutSeconds a <> configShutdownTimeoutSeconds b,
        configShutdownMode = configShutdownMode a <> configShutdownMode b,
        configStdout = configStdout a <> configStdout b,
        configStderr = configStderr a <> configStderr b
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
      { configPort = Last Nothing,
        configDatabaseName = "",
        configUser = "",
        configPassword = Nothing,
        configDataDirectory = DirectoryTemporary,
        configSocketDirectory = DirectoryTemporary,
        configTemporaryRoot = Last Nothing,
        configPostgresSettings = [],
        configInitDbArgs = [],
        configPostgresArgs = [],
        configCreateDbArgs = [],
        configConnectionTimeoutSeconds = Last Nothing,
        configShutdownTimeoutSeconds = Last Nothing,
        configShutdownMode = Last Nothing,
        configStdout = Last Nothing,
        configStderr = Last Nothing
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
    { configPort = Last Nothing,
      configDatabaseName = "postgres",
      configUser = "", -- Will use current user
      configPassword = Nothing,
      configDataDirectory = DirectoryTemporary,
      configSocketDirectory = DirectoryTemporary,
      configTemporaryRoot = Last Nothing,
      configPostgresSettings = defaultPostgresSettings,
      configInitDbArgs = defaultInitDbArgs,
      configPostgresArgs = [],
      configCreateDbArgs = [],
      configConnectionTimeoutSeconds = Last (Just defaultConnectionTimeoutSeconds),
      configShutdownTimeoutSeconds = Last (Just defaultShutdownTimeoutSeconds),
      configShutdownMode = Last (Just ShutdownGraceful),
      configStdout = Last (Just Nothing), -- Discard by default
      configStderr = Last (Just Nothing) -- Discard by default
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
    { configPostgresSettings =
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
    { configPostgresSettings =
        [ ("shared_preload_libraries", "'auto_explain'"),
          ("auto_explain.log_min_duration", "'" <> ms <> "'"),
          ("auto_explain.log_analyze", "'on'")
        ]
    }
  where
    ms :: Text
    ms = T.pack (show minDurationMs) <> "ms"
