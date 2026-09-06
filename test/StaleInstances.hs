module StaleInstances (spec, childMode) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, throwTo)
import Control.Exception (AsyncException (ThreadKilled), bracket, finally, throwIO, try)
import Control.Monad (forM_, forever, when)
import Data.IORef
import Data.Monoid (Last (..))
import EphemeralPg qualified as Pg
import EphemeralPg.Config qualified as Config
import EphemeralPg.Database (PostgresProcess (..))
import EphemeralPg.Internal.Cache (getCacheDirectory, getCacheKey)
import EphemeralPg.Internal.Instance
import EphemeralPg.Internal.ProcessIdentity
import EphemeralPg.Internal.Sweep qualified as Sweep
import Hasql.Connection qualified as Connection
import System.Directory qualified as D
import System.Environment (getEnv, getExecutablePath, setEnv)
import System.FilePath
import System.IO
import System.IO.Temp (withTempDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.User (getEffectiveUserID)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

childMode :: [String] -> IO Bool
childMode ["--stale-lock-child", root] = do
  (path, lease) <- registerInstance root
  putStrLn path
  hFlush stdout
  forever (threadDelay 1000000) `finally` releaseInstance lease
childMode ["--stale-db-child", root, mode] = do
  let config = Pg.defaultConfig {Pg.temporaryRoot = Last (Just root)}
      cache = Pg.defaultCacheConfig {Pg.root = Just (root </> "templates"), Pg.enabled = mode /= "disabled"}
      launch = if mode `elem` ["uncached", "legacy"] then Pg.start config else Pg.startCached config cache
  when (mode `elem` ["warm", "fallback"]) $ launch >>= either (fail . show) Pg.stop
  when (mode == "fallback") $ do
    let bin = root </> "bin"
    D.createDirectory bin
    writeFile (bin </> "cp") "#!/bin/sh\nexit 1\n"
    setFileMode (bin </> "cp") 0o700
    previous <- getEnv "PATH"
    setEnv "PATH" (bin <> ":" <> previous)
  db <- launch >>= either (fail . show) pure
  print (db.dataDirectory, db.process.pid)
  hFlush stdout
  forever (threadDelay 1000000) `finally` Pg.stop db
childMode _ = pure False

spec :: Spec
spec = describe "Stale instances" $ do
  it "excludes same-process claims and survives directory replacement" $
    withTempDirectory "/tmp" "epg" $ \rawRoot -> do
      root <- D.canonicalizePath rawRoot
      (path, lease) <- registerInstance root
      registry <- registryFor root
      let claim = claimInstance registry path $ \lock -> (lock == Nothing) `shouldBe` True
      ( do
          claim
          D.removeDirectory path
          claim
          D.createDirectory path
          claim
        )
        `finally` releaseInstance lease
      claimInstance registry path $ \lock -> (lock /= Nothing) `shouldBe` True
      releaseInstance lease
  it "releases ownership after a consumer is SIGKILLed" $
    withTempDirectory "/tmp" "epg" $ \rawRoot -> do
      root <- D.canonicalizePath rawRoot
      exe <- getExecutablePath
      bracket
        (createProcess (proc exe ["--stale-lock-child", root]) {std_out = CreatePipe})
        (\(_, output, _, process) -> terminateProcess process >> waitForProcess process >> mapM_ hClose output)
        $ \(_, output, _, process) -> case output of
          Nothing -> expectationFailure "Missing child pipe"
          Just handle -> do
            path <- timeout 5000000 (hGetLine handle) >>= maybe (fail "Child readiness timed out") pure
            registry <- registryFor root
            claimInstance registry path $ \lock -> (lock == Nothing) `shouldBe` True
            pid <- getPid process >>= maybe (fail "Missing child PID") pure
            signalProcess sigKILL pid
            _ <- waitForProcess process
            claimInstance registry path $ \lock -> (lock /= Nothing) `shouldBe` True
  it "classifies this live process without guessing identity" $ do
    pid <- getProcessID
    observation <- systemInspector.inspect pid
    case observation of
      Present ident -> ident.pid `shouldBe` pid
      other -> expectationFailure (show other)
  it "rejects malformed and unsafe PID records" $ do
    mapM_
      (\p -> parsePidRecord (p <> "\n/tmp/data\n1\n5432\n/tmp\nlocalhost\n1 1\nready\n") `shouldBe` Nothing)
      ["-2", "0", "1", "999999999999999999999999999"]
    parsePidRecord "123\n/tmp/data\n1\n" `shouldBe` Nothing

  it "removes dead tracked initialization debris exactly once across concurrent sweeps" $
    fixture $ \config path -> do
      first <- newEmptyMVar
      second <- newEmptyMVar
      _ <- forkIO $ Sweep.sweepWith goneInspector config >>= putMVar first
      _ <- forkIO $ Sweep.sweepWith goneInspector config >>= putMVar second
      results <- (<>) <$> takeMVar first <*> takeMVar second
      [p | (p, Sweep.Removed) <- results] `shouldBe` [path]
      Sweep.sweepWith goneInspector config `shouldReturn` []
  it "preserves missing legacy PID files, symlinks, permanent paths and malformed metadata" $
    fixture $ \config path -> do
      registry <- registryFor (takeDirectory path)
      writeFile (recordPath registry path) "invalid"
      Sweep.sweepWith goneInspector config `shouldReturn` [(path, Sweep.Failed)]
      D.removeFile (recordPath registry path)
      Sweep.sweepWith goneInspector config `shouldReturn` [(path, Sweep.Uncertain)]
      let link = takeDirectory path </> "ephpg-data-link"
      D.createDirectoryLink path link
      outcomes <- Sweep.sweepWith goneInspector config {Config.dataDirectory = Pg.DirectoryPermanent path}
      outcomes `shouldSatisfy` elem (path, Sweep.Active)
      outcomes `shouldSatisfy` elem (link, Sweep.Failed)
      D.doesDirectoryExist path `shouldReturn` True
  it "preserves uncertain owners and failed process enumeration" $
    fixture $ \config path -> do
      let uncertain = goneInspector {inspect = \_ -> pure (Unknown "permission denied")}
      Sweep.sweepWith uncertain config `shouldReturn` [(path, Sweep.Uncertain)]
      Sweep.sweepWith goneInspector {enumerate = pure (Left "enumeration failed")} config
        `shouldReturn` [(path, Sweep.Uncertain)]
      D.doesDirectoryExist path `shouldReturn` True
  it "rejects symlinked metadata and lifetime lock files" $
    fixture $ \config path -> do
      registry <- registryFor (takeDirectory path)
      let metadata = recordPath registry path
          saved = metadata <> ".saved"
          lock = registry </> takeFileName path <.> "lock"
      D.renameFile metadata saved
      D.createFileLink saved metadata
      Sweep.sweepWith goneInspector config `shouldReturn` [(path, Sweep.Failed)]
      D.removeFile metadata
      D.renameFile saved metadata
      D.renameFile lock (lock <> ".saved")
      D.createFileLink (lock <> ".saved") lock
      Sweep.sweepWith goneInspector config `shouldReturn` [(path, Sweep.Failed)]
      D.doesDirectoryExist path `shouldReturn` True
  it "does not remove data when ownership metadata changes during inspection" $
    fixture $ \config path -> do
      registry <- registryFor (takeDirectory path)
      let inspector = goneInspector {inspect = \_ -> writeFile (recordPath registry path) "changed" >> pure Gone}
      Sweep.sweepWith inspector config `shouldReturn` [(path, Sweep.Failed)]
      D.doesDirectoryExist path `shouldReturn` True
  it "propagates asynchronous cancellation" $
    fixture $ \config _ -> do
      result <- try @AsyncException $ Sweep.sweepWith goneInspector {inspect = \_ -> throwIO ThreadKilled} config
      result `shouldBe` Left ThreadKilled
  it "rejects replacement of the candidate during inspection" $
    fixture $ \config path -> do
      changed <- newIORef False
      let inspector =
            goneInspector
              { inspect = \_ -> do
                  old <- atomicModifyIORef' changed (\x -> (True, x))
                  if old then pure () else D.renameDirectory path (path <> "-saved") >> D.createDirectory path
                  pure Gone
              }
      results <- Sweep.sweepWith inspector config
      results `shouldSatisfy` elem (path, Sweep.Failed)
      D.doesDirectoryExist path `shouldReturn` True

  it "never signals reused PIDs, changed start identities, or non-orphan legacy servers" $
    fixture $ \config path -> do
      uid <- getEffectiveUserID
      let ident =
            Identity
              12345
              1
              uid
              1700000000
              "/test/postgres"
              ("/test/postgres -D " <> path <> " -k /tmp/socket")
              (Just path)
              False
      writeFile (path </> "postmaster.pid") (pidText path)
      signals <- newIORef []
      let run current =
            Sweep.sweepWith
              goneInspector
                { inspect = \pid -> pure $ if pid == 12345 then Present current else Gone,
                  interrupt = \pid -> modifyIORef' signals (pid :)
                }
              config
      run ident {command = "/test/unrelated"} `shouldReturn` [(path, Sweep.Uncertain)]
      run (Identity ident.pid ident.parent ident.uid 1800000000 ident.command ident.arguments ident.workingDirectory ident.zombie) `shouldReturn` [(path, Sweep.Uncertain)]
      registry <- registryFor (takeDirectory path)
      D.removeFile (recordPath registry path)
      writeFile (path </> "PG_VERSION") "17\n"
      run ident {parent = 42} `shouldReturn` [(path, Sweep.Active)]
      readIORef signals `shouldReturn` []
  it "leaves a timed-out server intact without escalating" $
    fixture $ \config path -> do
      uid <- getEffectiveUserID
      let ident =
            Identity
              12345
              1
              uid
              1700000000
              "/test/postgres"
              ("/test/postgres -D " <> path <> " -k /tmp/socket")
              (Just path)
              False
      writeFile (path </> "postmaster.pid") (pidText path)
      signals <- newIORef []
      Sweep.sweepWith
        goneInspector
          { inspect = \pid -> pure $ if pid == 12345 then Present ident else Gone,
            enumerate = pure (Right [ident]),
            interrupt = \pid -> modifyIORef' signals (pid :)
          }
        config
        `shouldReturn` [(path, Sweep.TimedOut)]
      readIORef signals `shouldReturn` [12345]
      D.doesDirectoryExist path `shouldReturn` True
  it "preserves an orphaned initialization child" $
    fixture $ \config path -> do
      uid <- getEffectiveUserID
      let ident = Identity 12345 1 uid 1700000000 "/test/initdb" "initdb" (Just path) False
      Sweep.sweepWith goneInspector {enumerate = pure (Right [ident])} config
        `shouldReturn` [(path, Sweep.Uncertain)]

  it "combines the automatic-sweep setting with right-biased identity" $ do
    let enabled = mempty {Pg.sweepStaleOnStart = Last (Just True)}
        disabled = mempty {Pg.sweepStaleOnStart = Last (Just False)}
    (mempty <> disabled).sweepStaleOnStart `shouldBe` Last (Just False)
    (disabled <> mempty).sweepStaleOnStart `shouldBe` Last (Just False)
    (enabled <> disabled).sweepStaleOnStart `shouldBe` Last (Just False)
    Pg.defaultConfig.sweepStaleOnStart `shouldBe` Last (Just True)
  it "preserves permanent data through both startup variants" $
    withTempDirectory "/tmp" "epg" $ \rawRoot -> do
      root <- D.canonicalizePath rawRoot
      forM_ [False, True] $ \cached -> do
        let path = root </> ("ephpg-data-permanent-" <> show cached)
            config = Pg.defaultConfig {Pg.temporaryRoot = Last (Just root), Config.dataDirectory = Pg.DirectoryPermanent path}
            launch = if cached then Pg.startCached config Pg.defaultCacheConfig else Pg.start config
        bracket (launch >>= either (fail . show) pure) Pg.stop $ \db -> db.dataDirectory `shouldBe` path
        Pg.sweepStaleInstances config `shouldReturn` []
        D.doesFileExist (path </> "PG_VERSION") `shouldReturn` True
  it "cleans failed startup before and after PostgreSQL launch" $
    withTempDirectory "/tmp" "epg" $ \rawRoot -> do
      root <- D.canonicalizePath rawRoot
      let config = Pg.defaultConfig {Pg.temporaryRoot = Last (Just root)}
      forM_
        [ config {Pg.initDbArgs = ["--invalid-ephemeral-test"]},
          config {Config.databaseName = "broken", Pg.createDbArgs = ["--invalid-ephemeral-test"]}
        ]
        $ \broken -> do
          result <- Pg.start broken
          case result of Left _ -> pure (); Right db -> Pg.stop db >> expectationFailure "Expected startup failure"
          names <- D.listDirectory root
          filter (\name -> take 11 name == "ephpg-data-") names `shouldBe` []

  forM_ ["initdb", "cp", "createdb"] $ \tool ->
    it ("protects ownership and propagates cancellation at the " <> tool <> " startup barrier") $
      withTempDirectory "/tmp" "epg" $ \rawRoot -> do
        root <- D.canonicalizePath rawRoot
        let config = Pg.defaultConfig {Pg.temporaryRoot = Last (Just root), Config.databaseName = "barrierdb"}
            cache = Pg.defaultCacheConfig {Pg.root = Just (root </> "templates")}
            launch = if tool == "cp" then Pg.startCached config cache else Pg.start config
            bin = root </> "bin"
            marker = root </> "barrier"
        when (tool == "cp") $ launch >>= either (fail . show) Pg.stop
        D.createDirectory bin
        writeFile (bin </> tool) ("#!/bin/sh\nprintf ready > " <> show marker <> "\nexec sleep 60\n")
        setFileMode (bin </> tool) 0o700
        bracket (getEnv "PATH") (setEnv "PATH") $ \previous -> do
          setEnv "PATH" (bin <> ":" <> previous)
          done <- newEmptyMVar
          worker <- forkIO $ try @AsyncException launch >>= putMVar done
          let awaitBarrier = D.doesFileExist marker >>= \ready -> if ready then pure () else threadDelay 10000 >> awaitBarrier
          flip finally (throwTo worker ThreadKilled) $ do
            timeout 15000000 awaitBarrier >>= maybe (fail "Startup barrier timed out") pure
            Pg.sweepStaleInstances config `shouldReturn` []
            throwTo worker ThreadKilled
            result <- timeout 10000000 (takeMVar done) >>= maybe (fail "Cancellation cleanup timed out") pure
            case result of Left ThreadKilled -> pure (); _ -> expectationFailure "Cancellation was swallowed"
            names <- D.listDirectory root
            filter (\name -> take 11 name == "ephpg-data-") names `shouldBe` []

  forM_ ["uncached", "cold", "warm", "disabled", "fallback", "legacy"] $ \mode ->
    forM_ [False, True] $ \automatic ->
      it ("recovers " <> mode <> " orphan with " <> (if automatic then "startup" else "explicit sweep") <> " and preserves live connections") $
        withTempDirectory "/tmp" "epg" $ \rawRoot -> do
          root <- D.canonicalizePath rawRoot
          let config = Pg.defaultConfig {Pg.temporaryRoot = Last (Just root)}
          bracket (Pg.start config >>= either (fail . show) pure) Pg.stop $ \survivor -> do
            exe <- getExecutablePath
            bracket
              (createProcess (proc exe ["--stale-db-child", root, mode]) {std_out = CreatePipe})
              (\(_, output, _, process) -> terminateProcess process >> waitForProcess process >> mapM_ hClose output)
              $ \(_, output, _, process) -> case output of
                Nothing -> expectationFailure "Missing child pipe"
                Just handle -> do
                  line <- timeout 15000000 (hGetLine handle) >>= maybe (fail "Child readiness timed out") pure
                  let (path, pgPid) = read line
                  let cleanup = do
                        _ <- Sweep.sweepStaleInstances config
                        exists <- D.doesFileExist (path </> "postmaster.pid")
                        if exists
                          then do
                            _ <- readProcessWithExitCode "pg_ctl" ["-D", path, "-m", "fast", "-w", "stop"] ""
                            pure ()
                          else pure ()
                  ( do
                      record <- boundedRead (path </> "postmaster.pid") >>= maybe (fail "Bad PID record") pure . parsePidRecord
                      observed <- systemInspector.inspect pgPid
                      case observed of
                        Present ident -> do
                          uid <- getEffectiveUserID
                          (ident, matchesServer uid record ident) `shouldSatisfy` snd
                          ident.pid `shouldBe` record.pid
                          -- Report enough evidence to diagnose OS identity disagreements.
                          abs (ident.started - record.started) `shouldSatisfy` (<= 2)
                        other -> expectationFailure (show other)
                      pid <- getPid process >>= maybe (fail "Missing child PID") pure
                      signalProcess sigKILL pid
                      _ <- waitForProcess process
                      systemInspector.inspect pgPid >>= (\case Present _ -> pure (); other -> expectationFailure (show other))
                      when (mode == "legacy") $ do
                        registry <- registryFor root
                        D.removeFile (recordPath registry path)
                      -- Opt-out startup must leave the orphan available for explicit cleanup.
                      bracket (Pg.start config {Pg.sweepStaleOnStart = Last (Just False)} >>= either (fail . show) pure) Pg.stop $ \_ ->
                        D.doesDirectoryExist path `shouldReturn` True
                      if automatic
                        then bracket (Pg.start config >>= either (fail . show) pure) Pg.stop $ \_ -> pure ()
                        else Pg.sweepStaleInstances config `shouldReturn` [path]
                      when (mode `elem` ["cold", "warm", "fallback"]) $ do
                        key <- getCacheKey config >>= either (fail . show) pure
                        cacheDir <- getCacheDirectory key (Just (root </> "templates"))
                        version <- readFile (cacheDir </> "data" </> "PG_VERSION")
                        bracket (Pg.startCached config Pg.defaultCacheConfig {Pg.root = Just (root </> "templates")} >>= either (fail . show) pure) Pg.stop $ \_ -> pure ()
                        readFile (cacheDir </> "data" </> "PG_VERSION") `shouldReturn` version
                      connected <- Connection.acquire (Pg.connectionSettings survivor)
                      either (fail . show) Connection.release connected
                      D.doesDirectoryExist path `shouldReturn` False
                      Sweep.sweepStaleInstances config `shouldReturn` []
                      D.doesDirectoryExist survivor.dataDirectory `shouldReturn` True
                    )
                    `finally` cleanup

goneInspector :: Inspector
goneInspector =
  Inspector
    { inspect = \_ -> pure Gone,
      enumerate = pure (Right []),
      interrupt = \_ -> expectationFailure "Unexpected signal"
    }

fixture :: (Pg.Config -> FilePath -> IO a) -> IO a
fixture action = withTempDirectory "/tmp" "epg" $ \rawRoot -> do
  root <- D.canonicalizePath rawRoot
  (path, lease) <- registerInstance root
  registry <- registryFor root
  record <- readRecord registry path
  releaseInstance lease
  writeFile (recordPath registry path) (show record {owner = 999999})
  action Pg.defaultConfig {Pg.temporaryRoot = Last (Just root)} path

pidText :: FilePath -> String
pidText path = "12345\n" <> path <> "\n1700000000\n5432\n/tmp/socket\n127.0.0.1\n1 1\nready\n"
