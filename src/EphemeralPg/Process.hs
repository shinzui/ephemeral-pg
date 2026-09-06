-- | Common process utilities for PostgreSQL command execution.
module EphemeralPg.Process
  ( -- * Process execution
    runProcessCapture,

    -- * Utilities
    findExecutable,
    getCurrentUser,
  )
where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.IO (SeekMode (AbsoluteSeek), hFlush, hSeek)
import System.IO.Temp (withSystemTempFile)
import System.Posix.User (getEffectiveUserName)
import System.Process.Typed
  ( proc,
    runProcess,
    setStderr,
    setStdout,
    useHandleOpen,
  )

-- | Run a process and capture its output.
runProcessCapture ::
  -- | Executable
  FilePath ->
  -- | Arguments
  [String] ->
  -- | (exit code, stdout, stderr)
  IO (ExitCode, Text, Text)
runProcessCapture exe args =
  withSystemTempFile "ephpg-stdout" $ \outPath out -> do
    Dir.removeFile outPath
    withSystemTempFile "ephpg-stderr" $ \errPath err -> do
      Dir.removeFile errPath
      -- Anonymous file-backed output avoids pipe-reader cleanup waiting for a
      -- child that has not yet been terminated during asynchronous cancellation.
      -- Unlink before launching so SIGKILL cannot leave output files behind.
      let config = proc exe args & setStdout (useHandleOpen out) & setStderr (useHandleOpen err)
      exitCode <- runProcess config
      hFlush out
      hFlush err
      hSeek out AbsoluteSeek 0
      hSeek err AbsoluteSeek 0
      stdout <- BS.hGetContents out
      stderr <- BS.hGetContents err
      pure (exitCode, T.decodeUtf8Lenient stdout, T.decodeUtf8Lenient stderr)

-- | Find an executable in PATH.
findExecutable :: String -> IO (Maybe FilePath)
findExecutable = Dir.findExecutable

-- | Get the current effective username.
getCurrentUser :: IO Text
getCurrentUser = do
  result <- try getEffectiveUserName
  case result of
    Left (_ :: IOException) -> pure "postgres"
    Right name -> pure $ T.pack name
