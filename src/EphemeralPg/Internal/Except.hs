-- | ExceptT-based helpers for startup operations.
--
-- This module provides a cleaner way to sequence operations that can fail,
-- replacing deeply nested case statements with 'ExceptT' do-notation.
module EphemeralPg.Internal.Except
  ( -- * Startup monad
    Startup,
    runStartup,

    -- * Lifting operations
    liftE,
    liftMaybe,

    -- * Error handling
    onError,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT, throwE)
import EphemeralPg.Error (StartError)

-- | The startup monad: 'ExceptT' 'StartError' 'IO'.
--
-- This provides:
--
-- * Automatic short-circuiting on errors (like @Either@, but in @IO@)
-- * Clean @do@ notation for sequencing fallible operations
-- * @liftIO@ for embedding pure @IO@ actions
type Startup = ExceptT StartError IO

-- | Run a startup computation, returning @Either StartError a@.
runStartup :: Startup a -> IO (Either StartError a)
runStartup = runExceptT

-- | Lift an @IO (Either StartError a)@ into 'Startup'.
--
-- This is the most common lifting operation, used for functions that
-- already return @Either StartError@.
--
-- @
-- (dataDir, isTemp) <- liftE $ createTempDataDirectory mRoot
-- @
liftE :: IO (Either StartError a) -> Startup a
liftE = ExceptT

-- | Lift a 'Maybe' into 'Startup', using the given error if 'Nothing'.
--
-- @
-- postgresPath <- liftMaybe (PostgresStartError PostgresNotFound)
--   =<< liftIO (findExecutable "postgres")
-- @
liftMaybe :: StartError -> Maybe a -> Startup a
liftMaybe err = maybe (throwE err) pure

-- | Run a cleanup action if the computation fails, then re-throw the error.
--
-- This is used to ensure resources are cleaned up on failure:
--
-- @
-- (socketDir, socketDirIsTemp) <- liftE (createTempSocketDirectory mRoot)
--   \`onError\` when dataDirIsTemp (removeDirectoryIfExists dataDir)
-- @
onError :: ExceptT e IO a -> IO () -> ExceptT e IO a
onError action cleanup = catchE action $ \err -> do
  liftIO cleanup
  throwE err
