# Migrating from tmp-postgres to ephemeral-pg

This guide covers migrating from [tmp-postgres](https://hackage.haskell.org/package/tmp-postgres) to ephemeral-pg.

## Quick Summary

| tmp-postgres | ephemeral-pg |
|---|---|
| `Database.Postgres.Temp` | `EphemeralPg` |
| `DB` | `Database` |
| `toConnectionString` | `connectionSettings` (hasql) or `connectionString` (libpq) |
| `StartError` | `StartError` (richer error types) |
| `with` | `with` |
| `withConfig` | `withConfig` |
| `start` / `stop` | `start` / `stop` |
| `withDbCache` | `withCached` |
| `takeSnapshot` / `cleanupSnapshot` | `createSnapshot` / `deleteSnapshot` |

## Step 1: Update Dependencies

**cabal file:**

```diff
  build-depends:
-   tmp-postgres,
+   ephemeral-pg,
```

If you were pulling tmp-postgres from a `source-repository-package` in your `cabal.project`, remove that stanza. ephemeral-pg is on Hackage.

## Step 2: Update Imports

```diff
- import Database.Postgres.Temp (StartError, toConnectionString, with)
+ import EphemeralPg (StartError, connectionSettings, with)
```

For more specific imports:

```diff
- import Database.Postgres.Temp (Config (..), defaultConfig, verboseConfig, autoExplainConfig)
+ import EphemeralPg (Config (..), defaultConfig, verboseConfig, autoExplainConfig)
+ import EphemeralPg.Config (DirectoryConfig (..), ShutdownMode (..))
```

## Step 3: Replace Connection String with Connection Settings

The biggest API difference is how you connect to the database. tmp-postgres gives you a `ByteString` connection string; ephemeral-pg gives you hasql `Settings` directly.

### Before (tmp-postgres)

```haskell
import Database.Postgres.Temp (toConnectionString, with)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings

example = with $ \db -> do
  let connStr = toConnectionString db
      connSettings = Settings.connectionString (decodeUtf8 connStr)
  Right conn <- Connection.acquire connSettings
  -- ...
```

### After (ephemeral-pg)

```haskell
import EphemeralPg (connectionSettings, with)
import Hasql.Connection qualified as Connection

example = with $ \db -> do
  Right conn <- Connection.acquire (connectionSettings db)
  -- ...
```

No more `ByteString` encoding/decoding or manual `Settings.connectionString` conversion.

### If You Need a libpq Connection String

If you're using postgresql-simple or another libpq-based library instead of hasql:

```haskell
import EphemeralPg (connectionString, with)
import Data.Text.Encoding (encodeUtf8)

example = with $ \db -> do
  let connStr = encodeUtf8 (connectionString db)
  conn <- PG.connectPostgreSQL connStr
  -- ...
```

Note: `connectionString` returns `Text` (not `ByteString` like tmp-postgres's `toConnectionString`).

## Step 4: Update Database Handle Access

tmp-postgres uses accessor functions on an opaque `DB` type. ephemeral-pg uses a record with `OverloadedRecordDot` syntax.

```diff
- toDataDirectory db
+ db.dataDirectory

- toTemporaryDirectory db
+ db.socketDirectory
```

## Step 5: Update Configuration

The configuration API uses a similar pattern but with different field names.

### Before (tmp-postgres)

```haskell
import Database.Postgres.Temp (Config (..), ProcessConfig (..), defaultConfig)

let config = defaultConfig
      { port = pure 5433
      , initDbConfig = mempty
          { commandLine = mempty
              { keyBased = Map.fromList [("--encoding", Just "UTF8")]
              }
          }
      }
```

### After (ephemeral-pg)

```haskell
import EphemeralPg (defaultConfig)
import EphemeralPg.Config (Config (..))
import Data.Monoid (Last (..))

let config = defaultConfig
      { port = Last (Just 5433)
      , initDbArgs = ["--encoding=UTF8"]
      , postgresSettings = [("shared_buffers", "256MB")]
      }
```

ephemeral-pg's configuration is flatter and uses plain lists instead of nested `ProcessConfig` / `CommandLineArgs` types.

### Combining Configurations

Both libraries support `Semigroup` for combining configs. ephemeral-pg provides ready-made configs you can combine:

```haskell
-- Verbose logging
let config = defaultConfig <> verboseConfig

-- Auto-explain for slow queries (>100ms)
let config = defaultConfig <> autoExplainConfig 100
```

## Step 6: Update Caching

### Before (tmp-postgres)

```haskell
import Database.Postgres.Temp (withDbCache, withDbCacheConfig, CacheConfig (..), defaultCacheConfig)

withDbCache defaultCacheConfig $ \cache ->
  withConfig (cacheConfig cache) $ \db ->
    -- ...
```

### After (ephemeral-pg)

```haskell
import EphemeralPg (withCached, withCachedConfig)

-- Simple (uses defaults)
withCached $ \db ->
  -- ...

-- With custom config
withCachedConfig config defaultCacheConfig $ \db ->
  -- ...
```

ephemeral-pg collapses the two-step cache setup into a single function.

## Step 7: Update Snapshots

### Before (tmp-postgres)

```haskell
import Database.Postgres.Temp (withSnapshot, takeSnapshot, cleanupSnapshot)

withSnapshot defaultConfig $ \snapshot ->
  withConfig (snapshotConfig snapshot) $ \db ->
    -- ...
```

### After (ephemeral-pg)

```haskell
import EphemeralPg.Snapshot (createSnapshot, restoreSnapshot, deleteSnapshot)

with $ \db -> do
  -- Set up initial state...

  Right snapshot <- createSnapshot db
  -- Run destructive test...

  Right () <- restoreSnapshot snapshot db
  -- State is restored

  deleteSnapshot snapshot
```

ephemeral-pg snapshots are more flexible - you can create and restore them at any point during the database lifecycle.

## Step 8: Update Error Handling

tmp-postgres uses a single `StartError` constructor. ephemeral-pg provides structured error types with rendering:

```diff
- show err
+ EphemeralPg.renderStartError err
```

The `StartError` type is now a sum type with specific constructors:

```haskell
data StartError
  = InitDbError InitDbError
  | PostgresStartError PostgresError
  | CreateDbError CreateDbError
  | ConfigError ConfigError
  | ResourceError ResourceError
  | TimeoutError TimeoutError
```

You can pattern match on specific errors for better diagnostics:

```haskell
case err of
  ConfigError (SocketPathTooLong {path, pathLength, pathMaxLength}) ->
    putStrLn $ "Socket path too long: " <> path
  TimeoutError (ConnectionTimeout {durationSeconds}) ->
    putStrLn $ "Timed out after " <> show durationSeconds <> "s"
  other ->
    putStrLn $ renderStartError other
```

## Complete Before/After Example

### Before (tmp-postgres)

```haskell
module TestSetup where

import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as TE
import Database.Postgres.Temp (StartError, toConnectionString, with)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig

withTestDb :: (Pool.Pool -> IO a) -> IO (Either StartError a)
withTestDb action = with $ \db -> do
  let connStr = toConnectionString db
      connSettings = Settings.connectionString (TE.decodeUtf8 connStr)

  connResult <- Connection.acquire connSettings
  case connResult of
    Left err -> error $ "Connection failed: " <> show err
    Right conn -> do
      -- Run migrations...
      Connection.release conn

  pool <- Pool.acquire (PoolConfig.settings
    [ PoolConfig.size 10
    , PoolConfig.staticConnectionSettings connSettings
    ])
  result <- action pool
  Pool.release pool
  pure result
```

### After (ephemeral-pg)

```haskell
module TestSetup where

import EphemeralPg (StartError, connectionSettings, with)
import Hasql.Connection qualified as Connection
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig

withTestDb :: (Pool.Pool -> IO a) -> IO (Either StartError a)
withTestDb action = with $ \db -> do
  let connSettings = connectionSettings db

  connResult <- Connection.acquire connSettings
  case connResult of
    Left err -> error $ "Connection failed: " <> show err
    Right conn -> do
      -- Run migrations...
      Connection.release conn

  pool <- Pool.acquire (PoolConfig.settings
    [ PoolConfig.size 10
    , PoolConfig.staticConnectionSettings connSettings
    ])
  result <- action pool
  Pool.release pool
  pure result
```

Key differences:
- No `ByteString` / `Text` encoding dance
- `connectionSettings` returns hasql `Settings` directly
- No `Hasql.Connection.Settings` import needed
- `StartError` is now in `EphemeralPg` instead of `Database.Postgres.Temp`

## Removed Features

The following tmp-postgres features have no direct equivalent in ephemeral-pg:

- **`optionsToDefaultConfig`** - ephemeral-pg doesn't build configs from libpq option strings
- **`makeDataDirectoryPermanent`** - Use `DirectoryPermanent path` in config instead
- **`prettyPrintDB` / `prettyPrintConfig`** - Use `show` or the structured error rendering
- **`stopPostgres` / `withRestart`** - Use `stop` and `restart` instead
- **`cacheAction`** - No equivalent; use snapshots for similar behavior

## New Features in ephemeral-pg

Features not available in tmp-postgres:

- **`connectionSettings`** - Native hasql connection settings
- **`renderStartError` / `renderStopError`** - Human-readable error messages
- **`autoExplainConfig`** - Pre-configured auto_explain for query plan analysis
- **Dump/Restore** - `EphemeralPg.Dump` module for pg_dump/psql integration
- **`ShutdownMode`** - Control shutdown behavior (Graceful/Fast/Immediate)
- **Socket path validation** - Catches path-too-long errors at config time
