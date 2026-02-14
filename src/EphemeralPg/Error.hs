-- | Error types for ephemeral-pg.
--
-- This module provides a rich error hierarchy for diagnosing failures
-- when starting or stopping temporary PostgreSQL databases.
module EphemeralPg.Error
  ( -- * Top-level errors
    StartError (..),
    StopError (..),

    -- * Specific error types
    InitDbError (..),
    PostgresError (..),
    CreateDbError (..),
    ConfigError (..),
    ResourceError (..),
    TimeoutError (..),

    -- * Error rendering
    renderStartError,
    renderStopError,
  )
where

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import System.Exit (ExitCode (..))

-- | Top-level error when starting a database.
data StartError
  = -- | initdb failed to initialize the data directory
    InitDbError InitDbError
  | -- | PostgreSQL server failed to start
    PostgresStartError PostgresError
  | -- | createdb failed to create the database
    CreateDbError CreateDbError
  | -- | Configuration is invalid
    ConfigError ConfigError
  | -- | System resource error (permissions, disk space, etc.)
    ResourceError ResourceError
  | -- | Timeout waiting for database to be ready
    TimeoutError TimeoutError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Errors when stopping a database.
data StopError
  = -- | Failed to send shutdown signal
    ShutdownSignalFailed Int Text
  | -- | Graceful shutdown timed out, had to force kill
    ShutdownTimedOut Int
  | -- | Failed to clean up directories
    CleanupFailed FilePath Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Errors from initdb execution.
data InitDbError
  = -- | initdb executable not found in PATH
    InitDbNotFound
  | -- | PostgreSQL version too old
    InitDbVersionMismatch
      { expected :: Text,
        actual :: Text
      }
  | -- | initdb process failed
    InitDbFailed
      { exitCode :: ExitCode,
        stdout :: Text,
        stderr :: Text,
        command :: Text
      }
  | -- | Cannot write to data directory
    InitDbPermissionDenied FilePath
  deriving stock (Eq, Show)

-- | Errors from PostgreSQL server operations.
data PostgresError
  = -- | postgres executable not found in PATH
    PostgresNotFound
  | -- | Server process exited unexpectedly
    PostgresStartFailed
      { exitCode :: ExitCode,
        stdout :: Text,
        stderr :: Text,
        command :: Text
      }
  | -- | Could not connect to verify server is running
    PostgresConnectionFailed Text
  | -- | Server version doesn't match requirements
    PostgresVersionMismatch
      { expected :: Text,
        actual :: Text
      }
  deriving stock (Eq, Show)

-- | Errors from createdb execution.
data CreateDbError
  = -- | createdb executable not found in PATH
    CreateDbNotFound
  | -- | createdb process failed
    CreateDbFailed
      { exitCode :: ExitCode,
        stdout :: Text,
        stderr :: Text,
        command :: Text,
        name :: Text
      }
  | -- | Database with this name already exists
    DatabaseAlreadyExists Text
  deriving stock (Eq, Show)

-- | Configuration validation errors.
data ConfigError
  = -- | Port number is invalid or unavailable
    InvalidPort Word16 Text
  | -- | Database name is invalid
    InvalidDatabaseName Text Text
  | -- | Username is invalid
    InvalidUser Text Text
  | -- | Data directory path is invalid
    InvalidDataDirectory FilePath Text
  | -- | Socket directory path is invalid
    InvalidSocketDirectory FilePath Text
  | -- | Socket path exceeds Unix domain socket limit
    SocketPathTooLong
      { path :: FilePath,
        pathLength :: Int,
        pathMaxLength :: Int
      }
  | -- | Configuration options conflict with each other
    ConflictingConfig Text
  deriving stock (Eq, Show)

-- | System resource errors.
data ResourceError
  = -- | Cannot access a required path
    PermissionDenied FilePath Text
  | -- | Cannot create a directory
    DirectoryCreationFailed FilePath Text
  | -- | Directory should be empty but isn't
    DirectoryNotEmpty FilePath
  | -- | Not enough disk space
    DiskSpaceExhausted FilePath
  | -- | Too many open files
    FileDescriptorLimit
  | -- | Requested port is already in use
    PortInUse Word16
  | -- | Could not find a free port
    PortAllocationFailed Text
  deriving stock (Eq, Show)

-- | Timeout errors.
data TimeoutError
  = -- | Timed out waiting for PostgreSQL to accept connections
    ConnectionTimeout
      { durationSeconds :: Int,
        host :: Text,
        port :: Word16
      }
  | -- | Timed out waiting for graceful shutdown
    ShutdownTimeout
      { timeoutSeconds :: Int,
        pid :: Int
      }
  deriving stock (Eq, Show)

-- | Render a 'StartError' for display to users.
renderStartError :: StartError -> Text
renderStartError = \case
  InitDbError InitDbNotFound ->
    "initdb not found. Please ensure PostgreSQL is installed and initdb is in your PATH.\n"
      <> "On Debian/Ubuntu, you may need to add /usr/lib/postgresql/<version>/bin to PATH."
  InitDbError (InitDbVersionMismatch expected actual) ->
    "PostgreSQL version mismatch: expected " <> expected <> " but found " <> actual
  InitDbError (InitDbFailed _code _stdout stderr cmd) ->
    "initdb failed:\n  Command: " <> cmd <> "\n  Error: " <> stderr
  InitDbError (InitDbPermissionDenied path) ->
    "Permission denied: cannot write to " <> T.pack path
  PostgresStartError PostgresNotFound ->
    "postgres not found. Please ensure PostgreSQL is installed and postgres is in your PATH."
  PostgresStartError (PostgresStartFailed _code _stdout stderr cmd) ->
    "PostgreSQL server failed to start:\n  Command: " <> cmd <> "\n  Error: " <> stderr
  PostgresStartError (PostgresConnectionFailed msg) ->
    "Could not connect to PostgreSQL: " <> msg
  PostgresStartError (PostgresVersionMismatch expected actual) ->
    "PostgreSQL version mismatch: expected " <> expected <> " but found " <> actual
  CreateDbError CreateDbNotFound ->
    "createdb not found. Please ensure PostgreSQL is installed and createdb is in your PATH."
  CreateDbError (CreateDbFailed _code _stdout stderr cmd name) ->
    "createdb failed for database '" <> name <> "':\n  Command: " <> cmd <> "\n  Error: " <> stderr
  CreateDbError (DatabaseAlreadyExists name) ->
    "Database '" <> name <> "' already exists"
  ConfigError (InvalidPort port msg) ->
    "Invalid port " <> T.pack (show port) <> ": " <> msg
  ConfigError (InvalidDatabaseName name msg) ->
    "Invalid database name '" <> name <> "': " <> msg
  ConfigError (InvalidUser name msg) ->
    "Invalid username '" <> name <> "': " <> msg
  ConfigError (InvalidDataDirectory path msg) ->
    "Invalid data directory '" <> T.pack path <> "': " <> msg
  ConfigError (InvalidSocketDirectory path msg) ->
    "Invalid socket directory '" <> T.pack path <> "': " <> msg
  ConfigError (SocketPathTooLong path len maxLen) ->
    "Socket path too long ("
      <> T.pack (show len)
      <> " > "
      <> T.pack (show maxLen)
      <> " bytes): "
      <> T.pack path
      <> "\nTry using a shorter temporary directory path."
  ConfigError (ConflictingConfig msg) ->
    "Conflicting configuration: " <> msg
  ResourceError (PermissionDenied path msg) ->
    "Permission denied for '" <> T.pack path <> "': " <> msg
  ResourceError (DirectoryCreationFailed path msg) ->
    "Failed to create directory '" <> T.pack path <> "': " <> msg
  ResourceError (DirectoryNotEmpty path) ->
    "Directory not empty: " <> T.pack path
  ResourceError (DiskSpaceExhausted path) ->
    "Disk space exhausted at " <> T.pack path
  ResourceError FileDescriptorLimit ->
    "Too many open files. Try increasing your system's file descriptor limit."
  ResourceError (PortInUse port) ->
    "Port " <> T.pack (show port) <> " is already in use"
  ResourceError (PortAllocationFailed msg) ->
    "Could not allocate a free port: " <> msg
  TimeoutError (ConnectionTimeout secs _host port) ->
    "Timed out after " <> T.pack (show secs) <> "s waiting for PostgreSQL on port " <> T.pack (show port)
  TimeoutError (ShutdownTimeout secs pid) ->
    "Timed out after " <> T.pack (show secs) <> "s waiting for process " <> T.pack (show pid) <> " to shut down"

-- | Render a 'StopError' for display to users.
renderStopError :: StopError -> Text
renderStopError = \case
  ShutdownSignalFailed pid msg ->
    "Failed to send shutdown signal to process " <> T.pack (show pid) <> ": " <> msg
  ShutdownTimedOut secs ->
    "Graceful shutdown timed out after " <> T.pack (show secs) <> "s, process was killed"
  CleanupFailed path msg ->
    "Failed to clean up '" <> T.pack path <> "': " <> msg
