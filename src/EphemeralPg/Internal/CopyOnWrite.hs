-- | Copy-on-write detection and operations.
--
-- This module provides copy-on-write support for efficient directory copying.
-- On macOS, this uses @clonefile()@ via @cp -c@. On Linux with Btrfs/XFS,
-- this uses @cp --reflink=always@.
--
-- Detection is done by actually attempting a clone operation on a test file,
-- so it works correctly regardless of filesystem type.
module EphemeralPg.Internal.CopyOnWrite
  ( -- * Capability detection
    CowCapability (..),
    CowMethod (..),
    detectCowCapability,

    -- * Directory operations
    copyDirectory,
    copyDirectoryCoW,
    copyDirectoryRegular,
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Process.Typed (nullStream, proc, runProcess, runProcess_, setStderr)

-- | Copy-on-write capability for a filesystem.
data CowCapability
  = -- | CoW is supported with the given method
    CowSupported CowMethod
  | -- | CoW is not supported, with reason
    CowNotSupported Text
  deriving stock (Eq, Show)

-- | The method used for copy-on-write.
data CowMethod
  = -- | macOS clonefile via cp -c
    CowClonefile
  | -- | Linux reflink via cp --reflink
    CowReflink
  deriving stock (Eq, Show)

-- | Detect copy-on-write capability for a path.
--
-- This tests the actual filesystem by attempting a clone operation.
-- The test is performed once and the result can be cached.
--
-- On macOS: Tests @cp -c@ (uses clonefile syscall)
-- On Linux: Tests @cp --reflink=always@
detectCowCapability :: FilePath -> IO CowCapability
detectCowCapability _testDir = do
  withSystemTempDirectory "cow-test-" $ \tmpDir -> do
    let srcFile = tmpDir </> "src"
    let dstFile = tmpDir </> "dst"

    -- Create a small test file
    writeFile srcFile "test"

    -- Try the platform-specific clone command
    result <- tryClone srcFile dstFile

    -- Clean up
    removeFile srcFile `catch_` pure ()
    removeFile dstFile `catch_` pure ()

    pure result
  where
    catch_ :: IO a -> IO a -> IO a
    catch_ action fallback = do
      result <- try @SomeException action
      case result of
        Left _ -> fallback
        Right a -> pure a

    tryClone :: FilePath -> FilePath -> IO CowCapability
    tryClone src dst = case os of
      "darwin" -> tryMacOSClone src dst
      "linux" -> tryLinuxReflink src dst
      _ -> pure $ CowNotSupported $ "Unsupported OS: " <> T.pack os

    tryMacOSClone :: FilePath -> FilePath -> IO CowCapability
    tryMacOSClone src dst = do
      -- cp -c uses clonefile() on macOS
      exitCode <- runProcess $ setStderr nullStream $ proc "cp" ["-c", src, dst]
      case exitCode of
        ExitSuccess -> pure $ CowSupported CowClonefile
        ExitFailure _ -> pure $ CowNotSupported "clonefile not supported on this filesystem"

    tryLinuxReflink :: FilePath -> FilePath -> IO CowCapability
    tryLinuxReflink src dst = do
      -- cp --reflink=always fails if reflink is not supported
      exitCode <- runProcess $ setStderr nullStream $ proc "cp" ["--reflink=always", src, dst]
      case exitCode of
        ExitSuccess -> pure $ CowSupported CowReflink
        ExitFailure _ -> pure $ CowNotSupported "reflink not supported on this filesystem"

-- | Copy a directory, using copy-on-write if the capability is available.
copyDirectory :: CowCapability -> FilePath -> FilePath -> IO (Either Text ())
copyDirectory capability src dst = case capability of
  CowSupported method -> copyDirectoryCoW method src dst
  CowNotSupported _ -> copyDirectoryRegular src dst

-- | Copy a directory using copy-on-write.
copyDirectoryCoW :: CowMethod -> FilePath -> FilePath -> IO (Either Text ())
copyDirectoryCoW method src dst = do
  result <- try $ runCopy method
  case result of
    Left (_ :: SomeException) ->
      -- Fall back to regular copy on failure
      copyDirectoryRegular src dst
    Right () ->
      pure $ Right ()
  where
    runCopy :: CowMethod -> IO ()
    runCopy = \case
      CowClonefile ->
        -- macOS: use cp -cR for recursive copy-on-write
        runProcess_ $ proc "cp" ["-cR", src, dst]
      CowReflink ->
        -- Linux: use cp --reflink=auto -R (auto falls back gracefully)
        runProcess_ $ proc "cp" ["--reflink=auto", "-R", src, dst]

-- | Copy a directory using regular (non-CoW) copy.
copyDirectoryRegular :: FilePath -> FilePath -> IO (Either Text ())
copyDirectoryRegular src dst = do
  result <- try $ runProcess_ $ proc "cp" ["-R", src, dst]
  case result of
    Left (ex :: SomeException) ->
      pure $ Left $ T.pack $ show ex
    Right () ->
      pure $ Right ()
