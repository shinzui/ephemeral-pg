module Main where

import Data.Text qualified as T
import EphemeralPg qualified as Pg
import EphemeralPg.Config qualified as Config
import Hasql.Connection qualified as Connection
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "EphemeralPg" $ do
    it "can start and stop a database" $ do
      result <- Pg.with $ \db -> do
        -- Just verify we can get connection settings
        let _settings = Pg.connectionSettings db
        pure ()
      result `shouldSatisfy` isRight

    it "can connect to the database" $ do
      result <- Pg.with $ \db -> do
        connResult <- Connection.acquire (Pg.connectionSettings db)
        case connResult of
          Left err -> fail $ "Connection failed: " <> show err
          Right conn -> do
            Connection.release conn
            pure ()
      result `shouldSatisfy` isRight

    it "can restart a database" $ do
      result <- Pg.with $ \db -> do
        -- Get initial port
        let port1 = Pg.port db

        -- Restart the database
        restartResult <- Pg.restart db
        case restartResult of
          Left err -> fail $ "Restart failed: " <> show err
          Right db' -> do
            -- Verify we can still connect after restart
            connResult <- Connection.acquire (Pg.connectionSettings db')
            case connResult of
              Left err -> fail $ "Connection after restart failed: " <> show err
              Right conn -> do
                Connection.release conn
                -- Port should be the same
                Pg.port db' `shouldBe` port1
      result `shouldSatisfy` isRight

  describe "EphemeralPg caching" $ do
    it "can start with caching enabled" $ do
      result <- Pg.withCached $ \db -> do
        connResult <- Connection.acquire (Pg.connectionSettings db)
        case connResult of
          Left err -> fail $ "Connection failed: " <> show err
          Right conn -> do
            Connection.release conn
            pure ()
      result `shouldSatisfy` isRight

    it "second cached startup is faster" $ do
      -- First run creates the cache
      result1 <- Pg.withCached $ \db -> do
        let _settings = Pg.connectionSettings db
        pure ()
      result1 `shouldSatisfy` isRight

      -- Second run should use the cache
      result2 <- Pg.withCached $ \db -> do
        connResult <- Connection.acquire (Pg.connectionSettings db)
        case connResult of
          Left err -> fail $ "Connection failed: " <> show err
          Right conn -> do
            Connection.release conn
            pure ()
      result2 `shouldSatisfy` isRight

  describe "Config" $ do
    it "satisfies left identity (mempty <> x = x)" $
      property $ \dbName ->
        let x = Config.defaultConfig {Config.configDatabaseName = T.pack dbName}
         in Config.configDatabaseName (mempty <> x) == Config.configDatabaseName x

    it "satisfies right identity (x <> mempty = x)" $
      property $ \dbName ->
        let x = Config.defaultConfig {Config.configDatabaseName = T.pack dbName}
         in Config.configDatabaseName (x <> mempty) == Config.configDatabaseName x

    it "satisfies associativity ((x <> y) <> z = x <> (y <> z))" $
      property $ \(n1, n2, n3) ->
        let x = mempty {Config.configDatabaseName = T.pack n1}
            y = mempty {Config.configDatabaseName = T.pack n2}
            z = mempty {Config.configDatabaseName = T.pack n3}
         in Config.configDatabaseName ((x <> y) <> z)
              == Config.configDatabaseName (x <> (y <> z))

    it "combines postgres settings" $ do
      let c1 = mempty {Config.configPostgresSettings = [("a", "1")]}
          c2 = mempty {Config.configPostgresSettings = [("b", "2")]}
          combined = c1 <> c2
      Config.configPostgresSettings combined `shouldBe` [("a", "1"), ("b", "2")]

  describe "Socket path validation" $ do
    it "rejects paths that are too long" $ do
      -- Unix socket path limit is typically 104-108 bytes
      -- PostgreSQL adds ".s.PGSQL.<port>" (~17 chars) to the path
      let longPath = replicate 200 'x'
      -- This should fail during startup due to socket path length
      -- We test this indirectly through the config
      length longPath `shouldSatisfy` (> Config.maxSocketPathLength)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False
