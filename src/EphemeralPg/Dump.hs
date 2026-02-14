-- | Database dump and restore functionality.
--
-- This module provides wrappers around pg_dump and psql for
-- creating and restoring SQL dumps of ephemeral databases.
--
-- = Example Usage
--
-- @
-- result <- Pg.'with' $ \\db -> do
--   -- Set up some test data
--   runMigrations db
--
--   -- Create a dump
--   dumpResult <- Pg.'dump' db \"/path/to/dump.sql\"
--
--   -- Later, restore the dump to a new database
--   Pg.'with' $ \\newDb -> do
--     Pg.'restore' newDb \"/path/to/dump.sql\"
-- @
module EphemeralPg.Dump
  ( -- * Dump Operations
    dump,
    dumpToText,

    -- * Restore Operations
    restore,
    restoreFromText,

    -- * Configuration
    DumpFormat (..),
    DumpOptions (..),
    defaultDumpOptions,
  )
where

import Control.Exception (SomeException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import EphemeralPg.Database (Database (..))
import EphemeralPg.Process (findExecutable)
import System.Exit (ExitCode (..))
import System.Process.Typed
  ( byteStringOutput,
    proc,
    readProcess,
    setEnv,
    setStderr,
    setStdout,
  )

-- | Output format for pg_dump.
data DumpFormat
  = -- | Plain SQL text format
    DumpPlain
  | -- | Custom archive format (for pg_restore)
    DumpCustom
  | -- | Directory format
    DumpDirectory
  | -- | Tar archive format
    DumpTar
  deriving stock (Eq, Show)

-- | Options for pg_dump.
data DumpOptions = DumpOptions
  { -- | Output format (default: DumpPlain)
    format :: DumpFormat,
    -- | Include CREATE DATABASE command
    createDb :: Bool,
    -- | Schema only (no data)
    schemaOnly :: Bool,
    -- | Data only (no schema)
    dataOnly :: Bool,
    -- | Include specific tables (empty = all)
    tables :: [Text],
    -- | Exclude specific tables
    excludeTables :: [Text]
  }
  deriving stock (Eq, Show)

-- | Default dump options (plain SQL, all tables, schema + data).
defaultDumpOptions :: DumpOptions
defaultDumpOptions =
  DumpOptions
    { format = DumpPlain,
      createDb = False,
      schemaOnly = False,
      dataOnly = False,
      tables = [],
      excludeTables = []
    }

-- | Dump a database to a file.
dump :: Database -> FilePath -> DumpOptions -> IO (Either Text ())
dump db outputPath opts = do
  mPgDump <- findExecutable "pg_dump"
  case mPgDump of
    Nothing -> pure $ Left "pg_dump not found in PATH"
    Just pgDumpPath -> do
      let args = buildDumpArgs db opts <> ["-f", T.pack outputPath]
          env = buildEnv db
          processConfig =
            proc pgDumpPath (map T.unpack args)
              & setStdout byteStringOutput
              & setStderr byteStringOutput
              & setEnv env
      result <- try $ readProcess processConfig
      case result of
        Left (ex :: SomeException) ->
          pure $ Left $ "pg_dump failed: " <> T.pack (show ex)
        Right (exitCode, _stdout, stderr) ->
          case exitCode of
            ExitSuccess -> pure $ Right ()
            ExitFailure code ->
              pure $
                Left $
                  "pg_dump failed with code "
                    <> T.pack (show code)
                    <> ": "
                    <> T.decodeUtf8Lenient (LBS.toStrict stderr)
  where
    (&) = flip ($)

-- | Dump a database and return the dump as text.
--
-- Only works with DumpPlain format.
dumpToText :: Database -> DumpOptions -> IO (Either Text Text)
dumpToText db opts = do
  if opts.format /= DumpPlain
    then pure $ Left "dumpToText only works with DumpPlain format"
    else do
      mPgDump <- findExecutable "pg_dump"
      case mPgDump of
        Nothing -> pure $ Left "pg_dump not found in PATH"
        Just pgDumpPath -> do
          let args = buildDumpArgs db opts
              env = buildEnv db
              processConfig =
                proc pgDumpPath (map T.unpack args)
                  & setStdout byteStringOutput
                  & setStderr byteStringOutput
                  & setEnv env
          result <- try $ readProcess processConfig
          case result of
            Left (ex :: SomeException) ->
              pure $ Left $ "pg_dump failed: " <> T.pack (show ex)
            Right (exitCode, stdout, stderr) ->
              case exitCode of
                ExitSuccess -> pure $ Right $ T.decodeUtf8Lenient $ LBS.toStrict stdout
                ExitFailure code ->
                  pure $
                    Left $
                      "pg_dump failed with code "
                        <> T.pack (show code)
                        <> ": "
                        <> T.decodeUtf8Lenient (LBS.toStrict stderr)
  where
    (&) = flip ($)

-- | Restore a database from a dump file.
restore :: Database -> FilePath -> IO (Either Text ())
restore db inputPath = do
  mPsql <- findExecutable "psql"
  case mPsql of
    Nothing -> pure $ Left "psql not found in PATH"
    Just psqlPath -> do
      let args =
            [ "-h",
              T.pack db.socketDirectory,
              "-p",
              T.pack $ show db.port,
              "-U",
              db.user,
              "-d",
              db.databaseName,
              "-f",
              T.pack inputPath,
              "-v",
              "ON_ERROR_STOP=1"
            ]
          env = buildEnv db
          processConfig =
            proc psqlPath (map T.unpack args)
              & setStdout byteStringOutput
              & setStderr byteStringOutput
              & setEnv env
      result <- try $ readProcess processConfig
      case result of
        Left (ex :: SomeException) ->
          pure $ Left $ "psql restore failed: " <> T.pack (show ex)
        Right (exitCode, _stdout, stderr) ->
          case exitCode of
            ExitSuccess -> pure $ Right ()
            ExitFailure code ->
              pure $
                Left $
                  "psql restore failed with code "
                    <> T.pack (show code)
                    <> ": "
                    <> T.decodeUtf8Lenient (LBS.toStrict stderr)
  where
    (&) = flip ($)

-- | Restore a database from SQL text.
restoreFromText :: Database -> Text -> IO (Either Text ())
restoreFromText db sqlText = do
  mPsql <- findExecutable "psql"
  case mPsql of
    Nothing -> pure $ Left "psql not found in PATH"
    Just psqlPath -> do
      let args =
            [ "-h",
              T.pack db.socketDirectory,
              "-p",
              T.pack $ show db.port,
              "-U",
              db.user,
              "-d",
              db.databaseName,
              "-c",
              sqlText,
              "-v",
              "ON_ERROR_STOP=1"
            ]
          env = buildEnv db
          processConfig =
            proc psqlPath (map T.unpack args)
              & setStdout byteStringOutput
              & setStderr byteStringOutput
              & setEnv env
      result <- try $ readProcess processConfig
      case result of
        Left (ex :: SomeException) ->
          pure $ Left $ "psql restore failed: " <> T.pack (show ex)
        Right (exitCode, _stdout, stderr) ->
          case exitCode of
            ExitSuccess -> pure $ Right ()
            ExitFailure code ->
              pure $
                Left $
                  "psql restore failed with code "
                    <> T.pack (show code)
                    <> ": "
                    <> T.decodeUtf8Lenient (LBS.toStrict stderr)
  where
    (&) = flip ($)

-- | Build pg_dump command line arguments.
buildDumpArgs :: Database -> DumpOptions -> [Text]
buildDumpArgs db opts =
  [ "-h",
    T.pack db.socketDirectory,
    "-p",
    T.pack $ show db.port,
    "-U",
    db.user,
    "-d",
    db.databaseName
  ]
    <> formatArg
    <> createDbArg
    <> schemaOnlyArg
    <> dataOnlyArg
    <> tableArgs
    <> excludeTableArgs
  where
    formatArg = case opts.format of
      DumpPlain -> ["-Fp"]
      DumpCustom -> ["-Fc"]
      DumpDirectory -> ["-Fd"]
      DumpTar -> ["-Ft"]

    createDbArg =
      if opts.createDb then ["-C"] else []

    schemaOnlyArg =
      if opts.schemaOnly then ["-s"] else []

    dataOnlyArg =
      if opts.dataOnly then ["-a"] else []

    tableArgs =
      concatMap (\t -> ["-t", t]) opts.tables

    excludeTableArgs =
      concatMap (\t -> ["-T", t]) opts.excludeTables

-- | Build environment variables for pg commands.
buildEnv :: Database -> [(String, String)]
buildEnv db =
  [("PGPASSWORD", T.unpack pw) | Just pw <- [db.password]]
