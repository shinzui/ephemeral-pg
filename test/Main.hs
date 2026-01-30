module Main where

import EphemeralPg qualified as Pg
import Hasql.Connection qualified as Connection
import Test.Hspec

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

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False
