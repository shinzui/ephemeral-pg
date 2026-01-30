-- | Copy-on-write detection and operations.
--
-- This module provides copy-on-write support for efficient directory copying.
-- On macOS, this uses @clonefile()@. On Linux with Btrfs/XFS, this uses
-- @FICLONE@ ioctl.
module EphemeralPg.Internal.CopyOnWrite
  ( -- * Capability detection
    CowCapability (..),
    detectCowCapability,

    -- * Directory operations
    copyDirectory,
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import System.Process.Typed (proc, runProcess_)

-- | Copy-on-write capability for a filesystem.
data CowCapability
  = -- | CoW is supported with the given method
    CowSupported Text
  | -- | CoW is not supported
    CowNotSupported Text
  deriving stock (Eq, Show)

-- | Detect copy-on-write capability for a path.
--
-- This is done once at startup by attempting to clone a test file.
-- For now, we use a simple heuristic based on the OS.
detectCowCapability :: FilePath -> IO CowCapability
detectCowCapability _path = do
  -- For simplicity, we'll try to use cp -c on macOS (which uses clonefile)
  -- and fall back to regular copy on other systems.
  -- A more sophisticated implementation would probe the filesystem.
  pure $ CowNotSupported "CoW detection not yet implemented, using regular copy"

-- | Copy a directory, using copy-on-write if available.
copyDirectory :: CowCapability -> FilePath -> FilePath -> IO (Either Text ())
copyDirectory capability src dst = do
  result <- try $ runCopy capability
  case result of
    Left (ex :: SomeException) ->
      pure $ Left $ T.pack $ show ex
    Right () ->
      pure $ Right ()
  where
    runCopy :: CowCapability -> IO ()
    runCopy = \case
      CowSupported "clonefile" ->
        -- macOS: use cp -c for copy-on-write
        runProcess_ $ proc "cp" ["-cR", src, dst]
      CowSupported _ ->
        -- Other CoW methods would go here
        runProcess_ $ proc "cp" ["-R", src, dst]
      CowNotSupported _ ->
        -- Regular copy
        runProcess_ $ proc "cp" ["-R", src, dst]
