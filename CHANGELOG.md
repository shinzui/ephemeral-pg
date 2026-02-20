# Changelog

## 0.2.1.0

### Bug Fixes

- Fix `stop` and `restart` ignoring configured shutdown mode and timeout — they
  were hardcoding `ShutdownGraceful` with a 30-second timeout instead of using
  the config values, causing `with`/`withCached` to block unnecessarily on shutdown

### Other Changes

- Change default shutdown mode to `ShutdownFast` (SIGINT) with a 5-second
  timeout, which is more appropriate for a testing library

## 0.2.0.1

### Bug Fixes

- Suppress pg_isready stdout/stderr during connection polling

## 0.2.0.0

### Breaking Changes

- Remove Hungarian-style field prefixes from all record types using
  `NoFieldSelectors`, `DuplicateRecordFields`, and `OverloadedRecordDot`
  - `Config`: `configPort` → `port`, `configDatabaseName` → `databaseName`, etc.
  - `Database`: `dbPort` → `port`, `dbDataDirectory` → `dataDirectory`, etc.
  - `PostgresProcess`: `postgresProcess` → `process`, `postgresPid` → `pid`
  - `DumpOptions`: `dumpFormat` → `format`, `dumpCreateDb` → `createDb`, etc.
  - `Snapshot`: `snapshotPath` → `path`, `snapshotTemporary` → `temporary`
  - `CacheKey`: `cacheKeyPgVersion` → `pgVersion`, `cacheKeyConfigHash` → `configHash`
  - `CacheConfig`: `cacheConfigRoot` → `root`, `cacheConfigCow` → `cow`, `cacheConfigEnabled` → `enabled`
  - Error types: all prefixed fields renamed similarly
- Remove accessor functions from `Database` module (`dataDirectory`, `socketDirectory`,
  `port`, `databaseName`, `user`); use dot syntax instead (e.g., `db.port`)
- `Database` is now exported with `(..)` from `EphemeralPg` for `HasField` resolution
- Consumers should enable `OverloadedRecordDot` to access record fields

## 0.1.0.0

- Initial release
- Core functionality: `with`, `withConfig`, `start`, `stop`
- Hasql integration via `connectionSettings`
- initdb caching support
- Copy-on-write support for macOS and Linux
- Snapshot support
