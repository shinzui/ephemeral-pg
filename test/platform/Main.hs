{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (forever, unless)
import Data.Monoid (Last (..))
import EphemeralPg.Config
import EphemeralPg.Internal.Instance
import EphemeralPg.Internal.ProcessIdentity
import EphemeralPg.Internal.Sweep
import System.Directory qualified as D
import System.Environment (getArgs, getExecutablePath)
import System.FilePath
import System.IO
import System.IO.Temp (withTempDirectory)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.User (getEffectiveUserID)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

main :: IO ()
main =
  getArgs >>= \case
    ["--child", root] -> do
      (path, lease) <- registerInstance root
      _ <- readProcess "initdb" ["-D", path, "--no-sync", "--auth=trust"] ""
      exe <- D.findExecutable "postgres" >>= maybe (fail "Missing postgres") pure
      _ <-
        createProcess
          (proc exe ["-D", path, "-k", root, "-h", "", "-p", "55439"])
            { std_out = UseHandle stderr,
              std_err = Inherit,
              std_in = Inherit,
              create_group = True
            }
      let ready = do
            (code, _, _) <- readProcessWithExitCode "pg_isready" ["-h", root, "-p", "55439"] ""
            if show code == "ExitSuccess" then pure () else threadDelay 50000 >> ready
      timeout 10000000 ready >>= maybe (fail "Server not ready") pure
      putStrLn path
      hFlush stdout
      forever (threadDelay 1000000) `finally` releaseInstance lease
    _ -> hspec $ describe "Stale instances platform" $ do
      it "releases a killed owner's lock while its exec child survives, verifies identity, and reaps" $
        withTempDirectory "/tmp" "epg" $ \rawRoot -> do
          root <- D.canonicalizePath rawRoot
          exe <- getExecutablePath
          bracket
            (createProcess (proc exe ["--child", root]) {std_out = CreatePipe})
            (\(_, output, _, process) -> terminateProcess process >> waitForProcess process >> mapM_ hClose output)
            $ \(_, output, _, process) -> do
              handle <- maybe (fail "Missing pipe") pure output
              path <- timeout 15000000 (hGetLine handle) >>= maybe (fail "Readiness timeout") pure
              let cleanup = do
                    exists <- D.doesFileExist (path </> "postmaster.pid")
                    if exists then readProcess "pg_ctl" ["-D", path, "-m", "fast", "-w", "stop"] "" >> pure () else pure ()
              flip finally cleanup $ do
                registry <- registryFor root
                claimInstance registry path $ \lock -> (lock == Nothing) `shouldBe` True
                pid <- getPid process >>= maybe (fail "Missing PID") pure
                signalProcess sigKILL pid
                _ <- waitForProcess process
                claimInstance registry path $ \lock -> (lock /= Nothing) `shouldBe` True
                record <- boundedRead (path </> "postmaster.pid") >>= maybe (fail "Bad PID record") pure . parsePidRecord
                uid <- getEffectiveUserID
                observed <- systemInspector.inspect record.pid
                print observed
                case observed of
                  Present ident -> matchesServer uid record ident `shouldBe` True
                  other -> fail (show other)
                outcomes <- sweepWith systemInspector defaultConfig {temporaryRoot = Last (Just root)}
                outcomes `shouldBe` [(path, Removed)]
                D.doesDirectoryExist path `shouldReturn` False
                sweepStaleInstances defaultConfig {temporaryRoot = Last (Just root)} `shouldReturn` []
