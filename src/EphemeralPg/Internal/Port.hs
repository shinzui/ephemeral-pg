-- | Port allocation for temporary PostgreSQL instances.
module EphemeralPg.Internal.Port
  ( -- * Port allocation
    findFreePort,
    isPortAvailable,
  )
where

import Control.Exception (SomeException, bracket, try)
import Data.Text qualified as T
import Data.Word (Word16)
import EphemeralPg.Error (ResourceError (..), StartError (..))
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketType (Stream),
    bind,
    close,
    socket,
    socketPort,
    tupleToHostAddress,
  )

-- | Find a free port by binding to port 0 and checking what the OS assigns.
findFreePort :: IO (Either StartError Word16)
findFreePort = do
  result <- try $ bracket openSocket close getAssignedPort
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ ResourceError $ PortAllocationFailed $ T.pack $ show e
    Right port ->
      pure $ Right port
  where
    openSocket :: IO Socket
    openSocket = do
      sock <- socket AF_INET Stream 0
      -- Bind to localhost on port 0 (OS assigns a free port)
      bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      pure sock

    getAssignedPort :: Socket -> IO Word16
    getAssignedPort sock = do
      port <- socketPort sock
      pure $ fromIntegral port

-- | Check if a specific port is available.
isPortAvailable :: Word16 -> IO Bool
isPortAvailable port = do
  result <- try $ bracket openSocket close (const $ pure ())
  case result of
    Left (_ :: SomeException) -> pure False
    Right () -> pure True
  where
    openSocket :: IO Socket
    openSocket = do
      sock <- socket AF_INET Stream 0
      bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
      pure sock
