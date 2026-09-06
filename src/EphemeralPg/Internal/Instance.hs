-- | Private, persistent ownership records. Lock paths are never recycled.
module EphemeralPg.Internal.Instance
  ( InstanceLease,
    Record (..),
    registerInstance,
    releaseInstance,
    registryFor,
    withRegistry,
    claimInstance,
    readRecord,
    recordPath,
    safeDirectory,
    safeFile,
    sameFile,
    boundedRead,
  )
where

import Control.Exception (IOException, bracket, catch, finally, mask_, onException)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.ByteString.Char8 qualified as BS
import Data.IORef
import System.Directory qualified as D
import System.FileLock
import System.FilePath
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.IO.Temp (createTempDirectory)
import System.Posix.Directory qualified as Posix
import System.Posix.Files
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Posix.User (getEffectiveUserID)
import Text.Read (readMaybe)

data Record = Record {version :: Int, dataPath :: FilePath, owner :: ProcessID, temporary :: Bool}
  deriving stock (Eq, Show, Read)

data InstanceLease = InstanceLease FilePath FilePath FileLock (IORef Bool)

safeDirectory :: FilePath -> IO FileStatus
safeDirectory path = do
  st <- getSymbolicLinkStatus path
  uid <- getEffectiveUserID
  unless (isDirectory st && fileOwner st == uid) $ ioError $ userError "Unsafe instance directory"
  pure st

safeFile :: FilePath -> IO FileStatus
safeFile path = do
  st <- getSymbolicLinkStatus path
  uid <- getEffectiveUserID
  unless (isRegularFile st && fileOwner st == uid && linkCount st == 1) $
    ioError $
      userError "Unsafe control file"
  pure st

sameFile :: FileStatus -> FileStatus -> Bool
sameFile a b = deviceID a == deviceID b && fileID a == fileID b

boundedRead :: FilePath -> IO String
boundedRead path = do
  before <- safeFile path
  bytes <- withBinaryFile path ReadMode $ \h -> BS.hGet h 8193
  after <- safeFile path
  unless (sameFile before after && BS.length bytes <= 8192) $ ioError $ userError "Changed or oversized control file"
  pure (BS.unpack bytes)

registryFor :: FilePath -> IO FilePath
registryFor root = do
  uid <- getEffectiveUserID
  let registry = root </> (".ephemeral-pg-instances-" <> show uid)
  Posix.createDirectory registry 0o700 `catch` \(e :: IOException) ->
    unless (isAlreadyExistsError e) (ioError e)
  st <- safeDirectory registry
  unless (fileMode st .&. 0o077 == 0) $ ioError $ userError "Instance registry must be private"
  pure registry

-- The enclosing directory is private; reject existing non-regular lock files.
checkLock :: FilePath -> IO ()
checkLock path =
  (safeFile path >> pure ()) `catch` \(e :: IOException) ->
    unless (isDoesNotExistError e) (ioError e)

withRegistry :: FilePath -> IO a -> IO a
withRegistry registry action = do
  _ <- safeDirectory registry
  let path = registry </> "registry.lock"
  checkLock path
  withFileLock path Exclusive $ \_ -> action

recordPath :: FilePath -> FilePath -> FilePath
recordPath registry path = registry </> takeFileName path <.> "record"

readRecord :: FilePath -> FilePath -> IO Record
readRecord registry path = do
  txt <- boundedRead (recordPath registry path)
  case readMaybe txt of
    Just r | r.version == 1 && r.dataPath == path && r.temporary && r.owner > 1 -> pure r
    _ -> ioError $ userError "Invalid instance record"

registerInstance :: FilePath -> IO (FilePath, InstanceLease)
registerInstance root = mask_ $ do
  registry <- registryFor root
  withRegistry registry $ do
    path <- createTempDirectory root "ephpg-data-"
    let lockPath = registry </> takeFileName path <.> "lock"
    checkLock lockPath
    acquired <- tryLockFile lockPath Exclusive
    lock <- maybe (ioError $ userError "New instance lock is busy") pure acquired
    let publish = do
          pid <- getProcessID
          let target = recordPath registry path
          writeFile (target <.> "new") (show (Record 1 path pid True))
          D.renameFile (target <.> "new") target
          ref <- newIORef False
          pure (path, InstanceLease registry path lock ref)
    publish `onException` unlockFile lock

-- Release is idempotent. Keep metadata and lock inode: a later sweep can retry
-- failed cleanup, and no waiter can accidentally acquire an obsolete inode.
releaseInstance :: InstanceLease -> IO ()
releaseInstance (InstanceLease registry path lock ref) = mask_ $ do
  released <- atomicModifyIORef' ref (\old -> (True, old))
  unless released $
    ( do
        exists <- D.doesPathExist path
        unless exists $
          withRegistry registry $
            D.removeFile (recordPath registry path) `catch` \(e :: IOException) ->
              unless (isDoesNotExistError e) (ioError e)
    )
      `finally` unlockFile lock

-- Claims are opened under the registry lock and held outside it. The record is
-- re-read by the caller while holding the lifetime lock.
claimInstance :: FilePath -> FilePath -> (Maybe FileLock -> IO a) -> IO a
claimInstance registry path = bracket acquire (mapM_ unlockFile)
  where
    acquire = withRegistry registry $ do
      let lockPath = registry </> takeFileName path <.> "lock"
      checkLock lockPath
      tryLockFile lockPath Exclusive
