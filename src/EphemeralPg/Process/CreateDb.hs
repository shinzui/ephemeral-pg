-- | createdb process execution.
module EphemeralPg.Process.CreateDb
  ( runCreateDb,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import EphemeralPg.Config (Config (..))
import EphemeralPg.Error (CreateDbError (..), StartError (..))
import EphemeralPg.Process (findExecutable, runProcessCapture)
import System.Exit (ExitCode (..))

-- | Run createdb to create a database.
runCreateDb ::
  Config ->
  -- | Socket directory
  FilePath ->
  -- | Port
  Word16 ->
  -- | Username
  Text ->
  -- | Database name
  Text ->
  IO (Either StartError ())
runCreateDb config socketDir port username dbName = do
  -- The "postgres" database is created by initdb, so we don't need to create it
  if dbName == "postgres"
    then pure $ Right ()
    else do
      -- Find createdb executable
      mCreateDb <- findExecutable "createdb"
      case mCreateDb of
        Nothing -> pure $ Left $ CreateDbError CreateDbNotFound
        Just createDbPath -> do
          -- Build arguments
          let args = buildCreateDbArgs config socketDir port username dbName

          -- Run createdb
          (exitCode, stdout, stderr) <- runProcessCapture createDbPath (map T.unpack args)

          case exitCode of
            ExitSuccess -> pure $ Right ()
            ExitFailure _ ->
              -- Check if it's because the database already exists
              if "already exists" `T.isInfixOf` stderr
                then pure $ Left $ CreateDbError $ DatabaseAlreadyExists dbName
                else
                  pure $
                    Left $
                      CreateDbError $
                        CreateDbFailed
                          { createDbExitCode = exitCode,
                            createDbStdout = stdout,
                            createDbStderr = stderr,
                            createDbCommand = T.unwords (T.pack createDbPath : args),
                            createDbName = dbName
                          }

-- | Build createdb command line arguments.
buildCreateDbArgs :: Config -> FilePath -> Word16 -> Text -> Text -> [Text]
buildCreateDbArgs config socketDir port username dbName =
  [ "--host=" <> T.pack socketDir,
    "--port=" <> T.pack (show port),
    "--username=" <> username,
    dbName
  ]
    <> configCreateDbArgs config
