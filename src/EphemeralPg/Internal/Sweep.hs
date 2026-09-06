-- | Conservative claims, bounded shutdown, and shutdown-before-deletion.
module EphemeralPg.Internal.Sweep (sweepStaleInstances, sweepWith, Outcome (..), directoryUnused) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Control.Monad (forM, unless)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Monoid (getLast)
import EphemeralPg.Config
import EphemeralPg.Internal.Instance
import EphemeralPg.Internal.ProcessIdentity
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory qualified as D
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)

data Outcome = Removed | Active | Uncertain | TimedOut | Failed deriving stock (Eq, Show)

-- | Reap provably abandoned immediate temporary data children. Ordinary I/O
-- failures skip candidates; asynchronous exceptions propagate.
sweepStaleInstances :: Config -> IO [FilePath]
sweepStaleInstances config = do
  outcomes <- sweepWith systemInspector config
  pure $ sort [path | (path, Removed) <- outcomes]

sweepWith :: Inspector -> Config -> IO [(FilePath, Outcome)]
sweepWith inspector config =
  ( do
      root <- maybe D.getTemporaryDirectory pure (getLast config.temporaryRoot) >>= D.canonicalizePath
      registry <- registryFor root
      excluded <- case config.dataDirectory of
        DirectoryTemporary -> pure Nothing
        DirectoryPermanent path -> Just <$> D.canonicalizePath path
      names <- D.listDirectory root
      forM (sort $ filter (isPrefixOf "ephpg-data-") names) $ \name -> do
        let path = root </> name
        outcome <-
          if Just path == excluded
            then pure Active
            else
              candidate inspector registry path `catch` \(_ :: IOException) -> pure Failed
        pure (path, outcome)
  )
    `catch` \(_ :: IOException) -> pure []

candidate :: Inspector -> FilePath -> FilePath -> IO Outcome
candidate inspector registry path = claimInstance registry path $ \case
  Nothing -> pure Active
  Just _ -> do
    original <- safeDirectory path
    canonical <- D.canonicalizePath path
    if canonical /= path
      then pure Uncertain
      else do
        tracked <-
          (Just <$> readRecord registry path) `catch` \(e :: IOException) ->
            if isDoesNotExistError e then pure Nothing else ioError e
        case tracked of
          Just record ->
            inspector.inspect record.owner >>= \case
              Gone -> examine True original (stableRecord (Just record))
              Present _ -> pure Active
              Unknown _ -> pure Uncertain
          Nothing -> do
            version <-
              boundedRead (path </> "PG_VERSION") `catch` \(e :: IOException) ->
                if isDoesNotExistError e then pure "" else ioError e
            if null version || any (\c -> c `notElem` ("0123456789.\n" :: String)) version
              then pure Uncertain
              else examine False original (stableRecord Nothing)
  where
    pidPath = path </> "postmaster.pid"
    readPid =
      (Just <$> boundedRead pidPath) `catch` \(e :: IOException) ->
        if isDoesNotExistError e then pure Nothing else ioError e
    stableRecord expected = do
      current <-
        (Just <$> readRecord registry path) `catch` \(e :: IOException) ->
          if isDoesNotExistError e then pure Nothing else ioError e
      if current /= expected
        then pure False
        else case current of
          Nothing -> pure True
          Just record -> (== Gone) <$> inspector.inspect record.owner
    examine tracked original stable = do
      contents <- readPid
      case contents of
        Nothing | tracked -> removeWhenUnused original Nothing stable
        Nothing -> pure Uncertain
        Just text -> case parsePidRecord text of
          Just record
            | record.path == path ->
                inspector.inspect record.pid >>= \case
                  Gone -> removeWhenUnused original (Just text) stable
                  Unknown _ -> pure Uncertain
                  Present ident -> do
                    uid <- getEffectiveUserID
                    if not (matchesServer uid record ident)
                      then pure Uncertain
                      else
                        if not tracked && ident.parent /= 1
                          then pure Active
                          else do
                            -- Revalidate filesystem, PID file and complete process identity.
                            unchanged <- sameFile original <$> safeDirectory path
                            current <- readPid
                            ownershipUnchanged <- stable
                            observed <- inspector.inspect record.pid
                            if not ownershipUnchanged || not unchanged || current /= Just text || observed /= Present ident
                              then pure Uncertain
                              else do
                                inspector.interrupt record.pid
                                deadline <- (+ 5000000000) <$> getMonotonicTimeNSec
                                let wait =
                                      inspector.inspect record.pid >>= \case
                                        Gone -> do
                                          result <- removeWhenUnused original Nothing stable
                                          clock <- getMonotonicTimeNSec
                                          if result == Uncertain && clock < deadline then threadDelay 50000 >> wait else pure result
                                        Present now | now.pid == ident.pid && now.started == ident.started && now.uid == ident.uid -> do
                                          clock <- getMonotonicTimeNSec
                                          if clock >= deadline then pure TimedOut else threadDelay 50000 >> wait
                                        Unknown _ -> do
                                          clock <- getMonotonicTimeNSec
                                          if clock >= deadline then pure Uncertain else threadDelay 50000 >> wait
                                        _ -> pure Uncertain
                                maybe TimedOut id <$> timeout 5000000 wait
          _ -> pure Uncertain
    removeWhenUnused original expected stable = do
      unused <- directoryUnused inspector path
      if not unused
        then pure Uncertain
        else do
          current <- readPid
          ownershipUnchanged <- stable
          -- After shutdown the PID file must be absent. A dead PID fixture may
          -- retain its exact original record, but may not acquire a new one.
          if not ownershipUnchanged || current /= expected
            then pure Uncertain
            else do
              fresh <- safeDirectory path
              unless (sameFile original fresh) $ ioError $ userError "Candidate was replaced"
              D.removeDirectoryRecursive path
              -- Persistent lock files prevent ABA claims; retire only metadata.
              withRegistry registry $
                D.removeFile (recordPath registry path) `catch` \(e :: IOException) ->
                  unless (isDoesNotExistError e) (ioError e)
              pure Removed

-- Any unclassified PostgreSQL launcher or initdb makes absence unprovable.
-- Inspect working directories even for workers with rewritten process titles.
-- Never infer absence from signal-zero alone.
directoryUnused :: Inspector -> FilePath -> IO Bool
directoryUnused inspector path =
  inspector.enumerate >>= \case
    Left _ -> pure False
    Right entries -> do
      uid <- getEffectiveUserID
      let own = filter (\entry -> entry.uid == uid && not entry.zombie) entries
          safe :: Identity -> Bool
          safe entry
            | takeFileName entry.command == "initdb" = False
            | isPostgres entry = case entry.workingDirectory of
                Just cwd -> cwd /= path && not (path `isInfixOf` entry.arguments)
                Nothing -> False
            | otherwise = True
      pure $ all safe own
