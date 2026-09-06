-- | Conservative process inspection. Unknown observations never authorize work.
module EphemeralPg.Internal.ProcessIdentity
  ( Observation (..),
    Identity (..),
    Inspector (..),
    systemInspector,
    PidRecord (..),
    parsePidRecord,
    matchesServer,
    isPostgres,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.ByteString.Char8 qualified as BS
import Data.List (intercalate, isPrefixOf)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.IO.Error (isDoesNotExistError)
import System.Info (os)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Signals (nullSignal, sigINT, signalProcess)
import System.Posix.Types (ProcessID, UserID)
import System.Posix.User (getEffectiveUserID)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)

data Identity = Identity
  { pid :: ProcessID,
    parent :: ProcessID,
    uid :: UserID,
    started :: Integer,
    command :: String,
    arguments :: String,
    workingDirectory :: Maybe FilePath,
    zombie :: Bool
  }
  deriving stock (Eq, Show)

data Observation = Gone | Present Identity | Unknown String deriving stock (Eq, Show)

data Inspector = Inspector
  { inspect :: ProcessID -> IO Observation,
    enumerate :: IO (Either String [Identity]),
    interrupt :: ProcessID -> IO ()
  }

data PidRecord = PidRecord {pid :: ProcessID, path :: FilePath, started :: Integer}
  deriving stock (Eq, Show)

parsePidRecord :: String -> Maybe PidRecord
parsePidRecord text = case lines text of
  p : path : start : port : socket : _listen : memory : status : _ -> do
    n <- readMaybe p :: Maybe Integer
    t <- readMaybe start
    portNumber <- readMaybe port :: Maybe Int
    if length text <= 8192
      && n > 1
      && n <= toInteger (maxBound :: ProcessID)
      && t > 0
      && not (null path)
      && portNumber > 0
      && portNumber <= 65535
      && not (null socket)
      && not (null memory)
      && words status `elem` [["ready"], ["starting"], ["stopping"]]
      then Just (PidRecord (fromInteger n) path t)
      else Nothing
  _ -> Nothing

matchesServer :: UserID -> PidRecord -> Identity -> Bool
matchesServer uid record ident =
  ident.pid == record.pid
    && ident.uid == uid
    && takeFileName ident.command == "postgres"
    && ident.workingDirectory == Just record.path
    && abs (ident.started - record.started) <= 2
    && (ident.command <> " -D " <> record.path <> " -k ") `isPrefixOf` ident.arguments

systemInspector :: Inspector
systemInspector = Inspector inspectProcess enumerateProcesses (signalProcess sigINT)

-- Linux installations such as NixOS provide procps through PATH.
psExecutable :: FilePath
psExecutable = if os == "darwin" then "/bin/ps" else "ps"

-- Only promote a platform after running the real orphan fixture there.
enumerateProcesses :: IO (Either String [Identity])
enumerateProcesses = observeProcesses Nothing

observeProcesses :: Maybe ProcessID -> IO (Either String [Identity])
observeProcesses target
  | os `notElem` ["darwin", "linux"] = pure $ Left "Process inspection has not been validated on this platform"
  | otherwise =
      ( do
          environment <- getEnvironment
          let fields = "pid=,ppid=,uid=,stat=,lstart=,comm="
              selection = case target of
                Nothing -> ["-ww", "-axo", fields]
                Just pid -> ["-ww", "-p", show pid, "-o", fields]
              cp =
                (proc psExecutable selection)
                  { env = Just (("LC_ALL", "C") : ("TZ", "UTC") : filter (\(k, _) -> k /= "LC_ALL" && k /= "TZ") environment)
                  }
          (code, output, _) <- readCreateProcessWithExitCode cp ""
          if code /= ExitSuccess && not (target /= Nothing && null output)
            then pure (Left "ps enumeration failed")
            else case traverse parseIdentity (lines output) of
              Nothing -> pure $ Left "Unparseable process metadata"
              Just entries -> do
                uid <- getEffectiveUserID
                let relevant = filter (\entry -> not entry.zombie && entry.uid == uid && (isPostgres entry || takeFileName entry.command == "initdb")) entries
                directories <-
                  if null relevant || os == "linux"
                    then pure []
                    else do
                      (cwdCode, cwdOutput, _) <-
                        readCreateProcessWithExitCode
                          (proc "/usr/sbin/lsof" ["-a", "-p", intercalate "," (map (show . (\entry -> entry.pid)) relevant), "-d", "cwd", "-Fn"])
                          ""
                      unless (cwdCode == ExitSuccess || not (null cwdOutput)) $ ioError $ userError "Cannot enumerate PostgreSQL working directories"
                      pure $ parseDirectories Nothing (lines cwdOutput)
                Right <$> traverse (\entry -> addArguments entry {workingDirectory = lookup entry.pid directories}) entries
      )
        `catch` \(e :: IOException) -> pure $ Left (show e)
  where
    addArguments ident
      | ident.zombie = pure ident
      | os == "linux" && (isPostgres ident || takeFileName ident.command == "initdb") = do
          uid <- getEffectiveUserID
          if ident.uid /= uid
            then pure ident
            else do
              let base = "/proc/" <> show ident.pid
              cwd <- readSymbolicLink (base <> "/cwd")
              exe <- readSymbolicLink (base <> "/exe")
              args <- BS.readFile (base <> "/cmdline")
              pure ident {command = exe, arguments = unwords (filter (not . null) $ map BS.unpack $ BS.split '\0' args), workingDirectory = Just cwd}
      | takeFileName ident.command `elem` ["postgres", "initdb"] = do
          (code, output, _) <-
            readCreateProcessWithExitCode
              (proc psExecutable ["-ww", "-p", show ident.pid, "-o", "args="])
              ""
          -- A disappearing entry invalidates this snapshot; the next sweep retries.
          if code == ExitSuccess && not (null output)
            then pure ident {arguments = unlinesTrim output}
            else ioError $ userError "Process changed during enumeration"
      | otherwise = pure ident
    unlinesTrim = reverse . dropWhile (== '\n') . reverse . dropWhile (== ' ')

parseIdentity :: String -> Maybe Identity
parseIdentity line = case words line of
  p : pp : user : state : day : month : date : clock : year : rest -> do
    pid <- readMaybe p
    parent <- readMaybe pp
    uid <- readMaybe user
    time <- parseTimeM True defaultTimeLocale "%a %b %e %T %Y" (unwords [day, month, date, clock, year]) :: Maybe UTCTime
    if null rest
      then Nothing
      else
        pure $
          Identity
            pid
            parent
            uid
            (floor $ utcTimeToPOSIXSeconds time)
            (unwords rest)
            ""
            Nothing
            ("Z" `isPrefixOf` state)
  _ -> Nothing

inspectProcess :: ProcessID -> IO Observation
inspectProcess pid
  | pid <= 1 = pure $ Unknown "Invalid PID"
  | otherwise = do
      snapshot <- observeProcesses (Just pid)
      case snapshot of
        Left reason -> pure $ Unknown reason
        Right entries -> case filter (\entry -> entry.pid == pid) entries of
          [entry] -> pure $ if entry.zombie then Gone else Present entry
          [] ->
            (signalProcess nullSignal pid >> pure (Unknown "Process appeared after enumeration"))
              `catch` \(e :: IOException) ->
                pure $
                  if isDoesNotExistError e then Gone else Unknown (show e)
          _ -> pure $ Unknown "Duplicate process identity"

isPostgres :: Identity -> Bool
isPostgres ident = takeFileName ident.command == "postgres" || "postgres: " `isPrefixOf` ident.command

parseDirectories :: Maybe ProcessID -> [String] -> [(ProcessID, FilePath)]
parseDirectories _ [] = []
parseDirectories _ (('p' : value) : rest) = parseDirectories (readMaybe value) rest
parseDirectories (Just pid) (('n' : path) : rest) = (pid, path) : parseDirectories (Just pid) rest
parseDirectories pid (_ : rest) = parseDirectories pid rest
