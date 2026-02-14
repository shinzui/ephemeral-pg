# ephemeral-pg

A modern Haskell library for creating temporary PostgreSQL databases for testing.

## Features

- Native hasql integration
- initdb caching for fast startup
- Copy-on-write support (macOS/Linux)
- Filesystem snapshots
- No shell injection vulnerabilities
- Type-safe configuration

## Requirements

- GHC 9.6+
- PostgreSQL 14+

## Installation

Add to your `cabal` file:

```cabal
build-depends:
  ephemeral-pg
```

Or with Stack, add to your `package.yaml`:

```yaml
dependencies:
  - ephemeral-pg
```

## Quick Start

```haskell
import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection

main :: IO ()
main = do
  result <- Pg.with \db -> do
    Right conn <- Connection.acquire (Pg.connectionSettings db)
    -- Use the connection...
    Connection.release conn
  case result of
    Left err -> putStrLn $ "Error: " <> Pg.renderStartError err
    Right () -> putStrLn "Success!"
```

## Usage Examples

### Basic Usage

The simplest way to use ephemeral-pg is with the `with` function, which creates
a temporary database, runs your action, and cleans up automatically:

```haskell
import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

testQuery :: IO ()
testQuery = do
  result <- Pg.with \db -> do
    Right conn <- Connection.acquire (Pg.connectionSettings db)

    -- Run a simple query
    result <- Session.run (Session.statement () selectOne) conn

    Connection.release conn
    pure result

  case result of
    Left err -> putStrLn $ "Startup error: " <> Pg.renderStartError err
    Right (Left sessionErr) -> putStrLn $ "Query error: " <> show sessionErr
    Right (Right value) -> putStrLn $ "Result: " <> show value
  where
    selectOne = Statement.Statement "SELECT 1" mempty decoder True
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4))
```

### Custom Configuration

You can customize the database configuration:

```haskell
import EphemeralPg qualified as Pg
import EphemeralPg.Config qualified as Config

customTest :: IO ()
customTest = do
  let config = Pg.defaultConfig
        { Config.configDatabaseName = "mytest"
        , Config.configPostgresSettings =
            [ ("log_statement", "'all'")
            , ("log_min_duration_statement", "0")
            ]
        }

  result <- Pg.withConfig config \db -> do
    -- Database name is "mytest"
    -- All statements are logged
    pure ()

  case result of
    Left err -> putStrLn $ Pg.renderStartError err
    Right () -> putStrLn "Done!"
```

### Using Verbose Configuration

For debugging, use the pre-configured verbose settings:

```haskell
import EphemeralPg qualified as Pg

debugTest :: IO ()
debugTest = do
  result <- Pg.withConfig Pg.verboseConfig \db -> do
    -- All statements logged with duration
    pure ()
  pure ()
```

### Using auto_explain

For query plan analysis:

```haskell
import EphemeralPg qualified as Pg

analyzeQueries :: IO ()
analyzeQueries = do
  result <- Pg.withConfig Pg.autoExplainConfig \db -> do
    -- Query plans automatically logged for slow queries
    pure ()
  pure ()
```

### Caching for Fast Startup

For faster test suite execution, use caching. The first run initializes the
cache, and subsequent runs copy from it (using CoW if available):

```haskell
import EphemeralPg qualified as Pg

cachedTest :: IO ()
cachedTest = do
  -- First call: ~2s (runs initdb and caches result)
  -- Subsequent calls: ~200ms (copies from cache)
  result <- Pg.withCached \db -> do
    -- Use the database...
    pure ()
  pure ()
```

### Manual Lifecycle Management

For more control, use `start` and `stop` directly:

```haskell
import EphemeralPg qualified as Pg
import Control.Exception (bracket)

manualLifecycle :: IO ()
manualLifecycle = do
  result <- Pg.start Pg.defaultConfig
  case result of
    Left err -> putStrLn $ Pg.renderStartError err
    Right db -> do
      -- Use the database...
      putStrLn $ "Database running on port: " <> show (Pg.port db)

      -- Clean up when done
      Pg.stop db
```

Or with bracket for exception safety:

```haskell
import EphemeralPg qualified as Pg
import Control.Exception (bracket)

bracketExample :: IO ()
bracketExample = do
  result <- Pg.start Pg.defaultConfig
  case result of
    Left err -> putStrLn $ Pg.renderStartError err
    Right db ->
      bracket (pure db) Pg.stop \db' -> do
        -- Use the database...
        pure ()
```

### Restarting the Database

You can restart the database to apply configuration changes or test recovery:

```haskell
import EphemeralPg qualified as Pg

restartTest :: IO ()
restartTest = do
  result <- Pg.with \db -> do
    let port1 = Pg.port db

    -- Restart the server
    restartResult <- Pg.restart db
    case restartResult of
      Left err -> fail $ "Restart failed: " <> show err
      Right db' -> do
        -- Server restarted, data preserved
        let port2 = Pg.port db'
        -- Port remains the same
        pure (port1 == port2)

  case result of
    Left err -> putStrLn $ Pg.renderStartError err
    Right same -> putStrLn $ "Ports same: " <> show same
```

### Snapshots

Create filesystem snapshots for fast test isolation:

```haskell
import EphemeralPg qualified as Pg
import EphemeralPg.Snapshot qualified as Snapshot

snapshotTest :: IO ()
snapshotTest = do
  result <- Pg.with \db -> do
    -- Set up initial state
    -- ... create tables, insert data ...

    -- Create a snapshot
    Right snapshot <- Snapshot.createSnapshot db

    -- Run a destructive test
    -- ... delete everything ...

    -- Restore to the snapshot
    Right () <- Snapshot.restoreSnapshot snapshot db

    -- Data is restored

    -- Clean up snapshot when done
    Snapshot.deleteSnapshot snapshot

    pure ()

  pure ()
```

### Dump and Restore

Export and import database contents:

```haskell
import EphemeralPg qualified as Pg
import EphemeralPg.Dump qualified as Dump

dumpTest :: IO ()
dumpTest = do
  result <- Pg.with \db -> do
    -- Set up data
    -- ...

    -- Dump to file
    Right () <- Dump.dump db "/tmp/backup.sql"

    pure ()

  -- Later, restore to a new database
  result2 <- Pg.with \db2 -> do
    Right () <- Dump.restore db2 "/tmp/backup.sql"
    -- Data is now in db2
    pure ()

  pure ()
```

## Configuration Reference

### Config Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `configDatabaseName` | `Text` | `"test"` | Name of the database to create |
| `configUser` | `Text` | Current user | PostgreSQL username |
| `configPassword` | `Text` | `""` | PostgreSQL password (empty = trust auth) |
| `configPort` | `Last Int` | Auto-assigned | Port number (finds free port if not specified) |
| `configDataDirectory` | `DirectoryConfig` | Temporary | Data directory location |
| `configSocketDirectory` | `DirectoryConfig` | Temporary | Unix socket directory |
| `configTemporaryRoot` | `Last FilePath` | System temp | Root for temporary directories |
| `configPostgresSettings` | `[(Text, Text)]` | Optimized defaults | postgresql.conf settings |
| `configInitDbArgs` | `[Text]` | `[]` | Additional initdb arguments |

### Pre-configured Configs

- `defaultConfig` - Basic configuration with optimized defaults
- `verboseConfig` - All statements logged with duration
- `autoExplainConfig` - Query plans logged for slow queries

### PostgreSQL Settings Defaults

The default configuration includes these optimized settings for testing:

```
shared_buffers = 12MB
fsync = off
synchronous_commit = off
full_page_writes = off
log_min_duration_statement = 0
log_connections = on
log_disconnections = on
random_page_cost = 1.0
```

## API Reference

### Core Functions

```haskell
-- Bracket-style (recommended)
with :: (Database -> IO a) -> IO (Either StartError a)
withConfig :: Config -> (Database -> IO a) -> IO (Either StartError a)

-- With caching
withCached :: (Database -> IO a) -> IO (Either StartError a)

-- Manual lifecycle
start :: Config -> IO (Either StartError Database)
stop :: Database -> IO ()
restart :: Database -> IO (Either StartError Database)
```

### Database Handle

```haskell
connectionSettings :: Database -> Settings  -- hasql Settings
connectionString :: Database -> Text         -- libpq connection string
dataDirectory :: Database -> FilePath
socketDirectory :: Database -> FilePath
port :: Database -> Int
databaseName :: Database -> Text
user :: Database -> Text
```

### Cache Management

```haskell
clearCache :: IO ()      -- Clear current user's cache
clearAllCaches :: IO ()  -- Clear all cached data
```

## Benchmarks

Measured with [tasty-bench](https://hackage.haskell.org/package/tasty-bench) in wall-clock mode on Apple Silicon.

| Benchmark | Time | vs baseline |
|---|---|---|
| **Lifecycle** | | |
| `defaultConfig` | 796 ms | -- |
| `defaultConfig <> verboseConfig` | 812 ms | 1.02x |
| `defaultConfig <> autoExplainConfig 100` | 769 ms | 0.97x |
| **Caching** | | |
| `withConfig` (uncached) | 755 ms | -- |
| `withCached` | 434 ms | 0.58x |
| **Snapshot** | | |
| lifecycle only | 761 ms | -- |
| lifecycle + `createSnapshot` | 1.13 s | 1.48x |
| lifecycle + `createSnapshot` + `restoreSnapshot` | 1.39 s | 1.83x |
| **Connection** | | |
| hasql `acquire` + `release` | 1.64 ms | -- |

Highlights:
- Cached startup is ~42% faster than uncached (CoW copy vs full initdb)
- `verboseConfig` and `autoExplainConfig` add negligible overhead
- Snapshot create adds ~365 ms, restore adds ~265 ms
- Connection acquire/release is ~1.6 ms once the database is running

## Comparison with tmp-postgres

ephemeral-pg is a modern replacement for tmp-postgres, addressing several issues:

| Feature | tmp-postgres | ephemeral-pg |
|---------|-------------|--------------|
| Shell injection | Vulnerable | Safe (typed-process) |
| Windows support | Partial | Unix-only (explicit) |
| Socket path length | Can fail | Validated |
| GHC version | 8.0+ | 9.6+ |
| hasql integration | Via options | Native |
| initdb caching | Yes | Yes (improved) |
| Copy-on-write | Yes | Yes |
| PostgreSQL version | 9.3+ | 14+ |

## Troubleshooting

### "initdb not found"

Ensure PostgreSQL binaries are in your PATH:

```bash
# macOS (Homebrew)
export PATH="/opt/homebrew/opt/postgresql@14/bin:$PATH"

# Linux (Debian/Ubuntu)
export PATH="/usr/lib/postgresql/14/bin:$PATH"
```

### Socket path too long

Unix sockets have a path length limit (~104 bytes). ephemeral-pg uses short
paths to avoid this, but if you specify a custom socket directory, ensure
the full path is under 90 characters.

### Permission denied

Ensure you have write access to the temporary directory. By default, the
system temp directory is used.

## License

BSD-3-Clause
