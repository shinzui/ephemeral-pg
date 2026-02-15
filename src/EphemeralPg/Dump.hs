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
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
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
dump db outputPath opts = runExceptT $ do
  pgDumpPath <- findExecutableE "pg_dump"
  let args = buildDumpArgs db opts <> ["-f", T.pack outputPath]
  _ <- runToolE "pg_dump" pgDumpPath args (buildEnv db)
  pure ()

-- | Dump a database and return the dump as text.
--
-- Only works with DumpPlain format.
dumpToText :: Database -> DumpOptions -> IO (Either Text Text)
dumpToText db opts = runExceptT $ do
  when (opts.format /= DumpPlain) $
    throwE "dumpToText only works with DumpPlain format"
  pgDumpPath <- findExecutableE "pg_dump"
  let args = buildDumpArgs db opts
  (stdout, _) <- runToolE "pg_dump" pgDumpPath args (buildEnv db)
  pure $ T.decodeUtf8Lenient $ LBS.toStrict stdout

-- | Restore a database from a dump file.
restore :: Database -> FilePath -> IO (Either Text ())
restore db inputPath = runExceptT $ do
  psqlPath <- findExecutableE "psql"
  let args = buildPsqlArgs db <> ["-f", T.pack inputPath]
  _ <- runToolE "psql restore" psqlPath args (buildEnv db)
  pure ()

-- | Restore a database from SQL text.
restoreFromText :: Database -> Text -> IO (Either Text ())
restoreFromText db sqlText = runExceptT $ do
  psqlPath <- findExecutableE "psql"
  let args = buildPsqlArgs db <> ["-c", sqlText]
  _ <- runToolE "psql restore" psqlPath args (buildEnv db)
  pure ()

-- | Find an executable in PATH or fail with a descriptive error.
findExecutableE :: String -> ExceptT Text IO FilePath
findExecutableE name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Nothing -> throwE $ T.pack name <> " not found in PATH"
    Just path -> pure path

-- | Run an external tool, handling exceptions and non-zero exit codes.
runToolE ::
  Text ->
  FilePath ->
  [Text] ->
  [(String, String)] ->
  ExceptT Text IO (LBS.ByteString, LBS.ByteString)
runToolE toolName exePath args env = do
  result <- liftIO $ try $ readProcess processConfig
  case result of
    Left (ex :: SomeException) ->
      throwE $ toolName <> " failed: " <> T.pack (show ex)
    Right (ExitSuccess, stdout, stderr) ->
      pure (stdout, stderr)
    Right (ExitFailure code, _, stderr) ->
      throwE $
        toolName
          <> " failed with code "
          <> T.pack (show code)
          <> ": "
          <> T.decodeUtf8Lenient (LBS.toStrict stderr)
  where
    processConfig =
      proc exePath (map T.unpack args)
        & setStdout byteStringOutput
        & setStderr byteStringOutput
        & setEnv env

-- | Common psql arguments for restore operations.
buildPsqlArgs :: Database -> [Text]
buildPsqlArgs db =
  [ "-h",
    T.pack db.socketDirectory,
    "-p",
    T.pack $ show db.port,
    "-U",
    db.user,
    "-d",
    db.databaseName,
    "-v",
    "ON_ERROR_STOP=1"
  ]

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
