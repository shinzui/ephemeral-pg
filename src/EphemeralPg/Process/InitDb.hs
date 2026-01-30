-- | initdb process execution.
module EphemeralPg.Process.InitDb
  ( -- * Initialization
    runInitDb,

    -- * Configuration file generation
    writePostgresConf,
  )
where

import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import EphemeralPg.Config (Config (..))
import EphemeralPg.Error (InitDbError (..), StartError (..))
import EphemeralPg.Process (findExecutable, getCurrentUser, runProcessCapture)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

-- | Run initdb to initialize a PostgreSQL data directory.
runInitDb :: Config -> FilePath -> IO (Either StartError ())
runInitDb config dataDir = do
  -- Find initdb executable
  mInitDb <- findExecutable "initdb"
  case mInitDb of
    Nothing -> pure $ Left $ InitDbError InitDbNotFound
    Just initDbPath -> do
      -- Get username
      username <- case configUser config of
        "" -> getCurrentUser
        u -> pure u

      -- Build arguments
      let args = buildInitDbArgs config dataDir username

      -- Run initdb
      (exitCode, stdout, stderr) <- runProcessCapture initDbPath (map T.unpack args)

      case exitCode of
        ExitSuccess -> do
          -- Write postgresql.conf with our settings
          writePostgresConf config dataDir
          pure $ Right ()
        ExitFailure _ ->
          pure $
            Left $
              InitDbError $
                InitDbFailed
                  { initDbExitCode = exitCode,
                    initDbStdout = stdout,
                    initDbStderr = stderr,
                    initDbCommand = T.unwords (T.pack initDbPath : args)
                  }

-- | Build initdb command line arguments.
buildInitDbArgs :: Config -> FilePath -> Text -> [Text]
buildInitDbArgs config dataDir username =
  [ "--pgdata=" <> T.pack dataDir,
    "--username=" <> username
  ]
    <> configInitDbArgs config

-- | Write postgresql.conf with the configured settings.
writePostgresConf :: Config -> FilePath -> IO ()
writePostgresConf config dataDir = do
  let confPath = dataDir </> "postgresql.conf"
  let settings = configPostgresSettings config
  let content = T.unlines $ map formatSetting settings
  T.appendFile confPath ("\n# ephemeral-pg settings\n" <> content)
  where
    formatSetting :: (Text, Text) -> Text
    formatSetting (key, value) = key <> " = " <> value
