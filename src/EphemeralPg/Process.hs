-- | Common process utilities for PostgreSQL command execution.
module EphemeralPg.Process
  ( -- * Process execution
    runProcessCapture,

    -- * Utilities
    findExecutable,
    getCurrentUser,
  )
where

import Control.Exception (SomeException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.Posix.User (getEffectiveUserName)
import System.Process.Typed
  ( byteStringOutput,
    proc,
    readProcess,
    setStderr,
    setStdout,
  )

-- | Run a process and capture its output.
runProcessCapture ::
  -- | Executable
  FilePath ->
  -- | Arguments
  [String] ->
  -- | (exit code, stdout, stderr)
  IO (ExitCode, Text, Text)
runProcessCapture exe args = do
  let config =
        proc exe args
          & setStdout byteStringOutput
          & setStderr byteStringOutput
  (exitCode, stdout, stderr) <- readProcess config
  pure
    ( exitCode,
      T.decodeUtf8Lenient $ LBS.toStrict stdout,
      T.decodeUtf8Lenient $ LBS.toStrict stderr
    )

-- | Find an executable in PATH.
findExecutable :: String -> IO (Maybe FilePath)
findExecutable = Dir.findExecutable

-- | Get the current effective username.
getCurrentUser :: IO Text
getCurrentUser = do
  result <- try getEffectiveUserName
  case result of
    Left (_ :: SomeException) -> pure "postgres"
    Right name -> pure $ T.pack name
