-- | PostgreSQL server process management.
module EphemeralPg.Process.Postgres
  ( -- * Server lifecycle
    startPostgres,
    stopPostgres,
    waitForPostgres,

    -- * Types
    PostgresProcess (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, mask_, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Function ((&))
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import EphemeralPg.Config
  ( Config (..),
    ShutdownMode (..),
    defaultConnectionTimeoutSeconds,
  )
import EphemeralPg.Database (PostgresProcess (..))
import EphemeralPg.Error
  ( PostgresError (..),
    StartError (..),
    StopError (..),
    TimeoutError (..),
  )
import EphemeralPg.Internal.Except (liftE, liftMaybe, onError, runStartup)
import EphemeralPg.Process (findExecutable)
import System.Exit (ExitCode (..))
import System.Posix.Signals (sigINT, sigKILL, sigQUIT, sigTERM, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (getPid)
import System.Process.Typed
  ( nullStream,
    proc,
    runProcess,
    setCreateGroup,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    unsafeProcessHandle,
    waitExitCode,
  )
import System.Timeout (timeout)

-- | Start the PostgreSQL server.
startPostgres ::
  Config ->
  -- | Data directory
  FilePath ->
  -- | Socket directory
  FilePath ->
  -- | Port
  Word16 ->
  -- | Username
  Text ->
  IO (Either StartError PostgresProcess)
startPostgres config dataDir socketDir port username = runStartup $ do
  -- Find postgres executable
  postgresPath <-
    liftMaybe (PostgresStartError PostgresNotFound)
      =<< liftIO (findExecutable "postgres")

  -- Build arguments and process config
  let args = buildPostgresArgs config dataDir socketDir port username
  let processConfig =
        proc postgresPath (map T.unpack args)
          & setStdin nullStream
          & setStdout nullStream
          & setStderr nullStream
          & setCreateGroup True -- So we can signal the whole group

  -- Start the process
  typedProcess <-
    liftIO (try $ startProcess processConfig) >>= \case
      Left (ex :: SomeException) ->
        throwE $
          PostgresStartError $
            PostgresStartFailed
              { exitCode = ExitFailure 1,
                stdout = "",
                stderr = T.pack $ show ex,
                command = T.unwords (T.pack postgresPath : args)
              }
      Right p -> pure p

  -- Get the PID
  let pHandle = unsafeProcessHandle typedProcess
  pgPid <-
    liftMaybe
      ( PostgresStartError $
          PostgresStartFailed
            { exitCode = ExitFailure 1,
              stdout = "",
              stderr = "Could not get process ID",
              command = T.unwords (T.pack postgresPath : args)
            }
      )
      =<< liftIO (getPid pHandle)

  let pgProcess =
        PostgresProcess
          { process = typedProcess,
            pid = CPid $ fromIntegral pgPid
          }

  -- Wait for the server to be ready
  let timeoutSecs =
        maybe defaultConnectionTimeoutSeconds id $
          getLast config.connectionTimeoutSeconds

  liftE (waitForPostgres socketDir port timeoutSecs)
    `onError` do
      -- Kill the server since it didn't start properly
      _ <- stopPostgres pgProcess ShutdownImmediate 5
      pure ()

  pure pgProcess

-- | Build postgres command line arguments.
buildPostgresArgs :: Config -> FilePath -> FilePath -> Word16 -> Text -> [Text]
buildPostgresArgs config dataDir socketDir port _username =
  [ "-D",
    T.pack dataDir,
    "-k",
    T.pack socketDir,
    "-p",
    T.pack (show port),
    "-h",
    "127.0.0.1" -- Also listen on TCP for debugging
  ]
    <> config.postgresArgs

-- | Wait for PostgreSQL to accept connections.
waitForPostgres :: FilePath -> Word16 -> Int -> IO (Either StartError ())
waitForPostgres socketDir port timeoutSecs = do
  let deadline = timeoutSecs * 1000000 -- Convert to microseconds
  result <- timeout deadline waitLoop
  case result of
    Nothing ->
      pure $
        Left $
          TimeoutError $
            ConnectionTimeout
              { durationSeconds = timeoutSecs,
                host = T.pack socketDir,
                port
              }
    Just () -> pure $ Right ()
  where
    waitLoop :: IO ()
    waitLoop = do
      -- Try to connect using pg_isready
      mPgIsReady <- findExecutable "pg_isready"
      case mPgIsReady of
        Nothing -> do
          -- Fall back to just waiting and hoping
          threadDelay 1000000 -- 1 second
          pure ()
        Just pgIsReadyPath -> do
          let args =
                [ "-h",
                  socketDir,
                  "-p",
                  show port,
                  "-t",
                  "1" -- 1 second timeout per attempt
                ]
          result <-
            try @SomeException $
              runProcess $
                proc pgIsReadyPath args
                  & setStdout nullStream
                  & setStderr nullStream
          case result of
            Right ExitSuccess -> pure ()
            _ -> do
              threadDelay 100000 -- 100ms between attempts
              waitLoop

-- | Stop the PostgreSQL server.
stopPostgres :: PostgresProcess -> ShutdownMode -> Int -> IO (Maybe StopError)
stopPostgres PostgresProcess {..} mode timeoutSecs = mask_ $ do
  let signal = case mode of
        ShutdownGraceful -> sigTERM
        ShutdownFast -> sigINT
        ShutdownImmediate -> sigQUIT

  -- Send the signal
  result <- try $ signalProcess signal pid
  case result of
    Left (_ :: SomeException) ->
      -- Process might already be dead
      pure Nothing
    Right () -> do
      -- Wait for the process to exit with timeout
      let deadline = timeoutSecs * 1000000
      exitResult <- timeout deadline $ waitExitCode process

      case exitResult of
        Just _ -> pure Nothing -- Exited normally
        Nothing -> do
          -- Timeout: force kill
          _ <- try @SomeException $ signalProcess sigKILL pid
          -- Wait a bit more for the forced kill
          _ <- timeout 5000000 $ waitExitCode process
          pure $ Just $ ShutdownTimedOut timeoutSecs
