-- | Directory management for temporary PostgreSQL instances.
module EphemeralPg.Internal.Directory
  ( -- * Directory creation
    createTempDataDirectory,
    createTempSocketDirectory,
    resolveDirectory,

    -- * Socket path validation
    validateSocketPath,
    estimateSocketPathLength,

    -- * Cleanup
    removeDirectoryIfExists,
    retryRemoveDirectory,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, try)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import EphemeralPg.Config (DirectoryConfig (..), maxSocketPathLength)
import EphemeralPg.Error (ConfigError (..), ResourceError (..), StartError (..))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
  )
import System.IO.Temp (createTempDirectory)

-- | Create a temporary data directory.
--
-- Returns the path and whether it should be cleaned up.
createTempDataDirectory :: Maybe FilePath -> IO (Either StartError (FilePath, Bool))
createTempDataDirectory mTempRoot = do
  tmpRoot <- maybe getTemporaryDirectory pure mTempRoot
  result <- try $ createTempDirectory tmpRoot "ephpg-data-"
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ ResourceError $ DirectoryCreationFailed tmpRoot (T.pack $ show e)
    Right path ->
      pure $ Right (path, True)

-- | Create a temporary socket directory.
--
-- Uses short names to avoid Unix socket path length limits.
-- Returns the path and whether it should be cleaned up.
createTempSocketDirectory :: Maybe FilePath -> IO (Either StartError (FilePath, Bool))
createTempSocketDirectory mTempRoot = do
  tmpRoot <- maybe getTemporaryDirectory pure mTempRoot
  -- Use very short prefix to minimize socket path length
  result <- try $ createTempDirectory tmpRoot "pg-"
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ ResourceError $ DirectoryCreationFailed tmpRoot (T.pack $ show e)
    Right path -> do
      -- Validate the socket path length
      case validateSocketPath path of
        Left err -> do
          -- Clean up the directory we just created
          removeDirectoryIfExists path
          pure $ Left err
        Right () ->
          pure $ Right (path, True)

-- | Resolve a directory configuration to an actual path.
--
-- Returns the path and whether it should be cleaned up.
resolveDirectory ::
  DirectoryConfig ->
  -- | Temp root
  Maybe FilePath ->
  -- | Directory purpose (for error messages)
  Text ->
  -- | Creator
  (Maybe FilePath -> IO (Either StartError (FilePath, Bool))) ->
  IO (Either StartError (FilePath, Bool))
resolveDirectory config mTempRoot _purpose creator =
  case config of
    DirectoryTemporary ->
      creator mTempRoot
    DirectoryPermanent path -> do
      result <- try $ createDirectoryIfMissing True path
      case result of
        Left (e :: SomeException) ->
          pure $ Left $ ResourceError $ DirectoryCreationFailed path (T.pack $ show e)
        Right () ->
          pure $ Right (path, False)

-- | Validate that a socket path won't exceed Unix limits.
--
-- The socket file will be named @.s.PGSQL.<port>@ which adds about 15 characters.
validateSocketPath :: FilePath -> Either StartError ()
validateSocketPath socketDir = do
  let estimatedLen = estimateSocketPathLength socketDir
  when (estimatedLen > maxSocketPathLength) $
    Left $
      ConfigError $
        SocketPathTooLong
          { socketPath = socketDir,
            socketPathLength = estimatedLen,
            socketPathMaxLength = maxSocketPathLength
          }

-- | Estimate the full socket path length.
--
-- PostgreSQL socket files are named @.s.PGSQL.<port>@.
-- Assuming max port 65535, the suffix is at most 17 characters.
estimateSocketPathLength :: FilePath -> Int
estimateSocketPathLength socketDir =
  length socketDir + 1 + 17 -- +1 for path separator, +17 for ".s.PGSQL.65535"

-- | Remove a directory if it exists, ignoring errors.
removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists path = do
  exists <- doesDirectoryExist path
  when exists $
    removeDirectoryRecursive path `catch` \(_ :: SomeException) -> pure ()

-- | Remove a directory with retries.
--
-- This handles the race condition where PostgreSQL may still be writing
-- files (like pg_stat) during shutdown.
retryRemoveDirectory :: FilePath -> Int -> Int -> IO (Either Text ())
retryRemoveDirectory path maxRetries delayMicros = go maxRetries
  where
    go 0 = do
      result <- try $ removeDirectoryRecursive path
      case result of
        Left (e :: SomeException) ->
          pure $ Left $ "Failed after " <> T.pack (show maxRetries) <> " retries: " <> T.pack (show e)
        Right () ->
          pure $ Right ()
    go n = do
      result <- try $ removeDirectoryRecursive path
      case result of
        Left (_ :: SomeException) -> do
          -- Wait and retry
          threadDelayMicros delayMicros
          go (n - 1)
        Right () ->
          pure $ Right ()

    threadDelayMicros :: Int -> IO ()
    threadDelayMicros = threadDelay
