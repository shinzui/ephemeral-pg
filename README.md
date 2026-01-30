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

## Quick Start

```haskell
import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session

main :: IO ()
main = do
  result <- Pg.with \db -> do
    Right conn <- Connection.acquire (Pg.connectionSettings db)
    -- Use the connection...
    Connection.release conn
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right () -> putStrLn "Success!"
```

## License

BSD-3-Clause
