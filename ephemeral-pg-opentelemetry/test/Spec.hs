-- | Programmatic tests for ephemeral-pg-opentelemetry.
--
-- Uses @hs-opentelemetry-exporter-in-memory@ to capture every emitted
-- 'ImmutableSpan' and asserts the resulting tree shape and attributes
-- without depending on stderr formatting.
module Main (main) where

import Data.IORef (readIORef)
import Data.Text (Text)
import OpenTelemetry.Attributes
  ( Attribute (..),
    PrimitiveAttribute (..),
    lookupAttribute,
  )
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Instrumentation.EphemeralPg
  ( defaultEphemeralPgOtelConfig,
    withTraced,
  )
import OpenTelemetry.Processor.Span (SpanProcessor)
import OpenTelemetry.Trace.Core
  ( ImmutableSpan (..),
    SpanContext (..),
    SpanStatus (..),
    TracerProvider,
    createTracerProvider,
    emptyTracerProviderOptions,
    getSpanContext,
    setGlobalTracerProvider,
    shutdownTracerProvider,
    tracerProviderOptionsSampler,
  )
import OpenTelemetry.Trace.Id (SpanId)
import OpenTelemetry.Trace.Sampler (alwaysOn)
import System.Environment (setEnv)
import Test.Hspec

main :: IO ()
main = do
  -- Opt in to both stable (v1.27+) and legacy DB attribute names so
  -- the assertions below can check either family. 'getSemanticsOptions'
  -- memoises on first call, so the env var must be set before any
  -- wrapper code runs.
  setEnv "OTEL_SEMCONV_STABILITY_OPT_IN" "http/dup"
  hspec $ describe "ephemeral-pg-opentelemetry" $ do
    it "stitches with under start/stop" $ do
      spans <- captureSpans $ do
        result <-
          withTraced defaultEphemeralPgOtelConfig $ \_db -> pure ()
        result `shouldBe` Right ()

      let withSpans = findSpansByName "ephemeralpg.with" spans
      let startSpans = findSpansByName "ephemeralpg.start" spans
      let stopSpans = findSpansByName "ephemeralpg.stop" spans
      let bodySpans = findSpansByName "ephemeralpg.with.body" spans

      length withSpans `shouldBe` 1
      length startSpans `shouldBe` 1
      length stopSpans `shouldBe` 1
      length bodySpans `shouldBe` 1

      let withId = headOrFail "ephemeralpg.with" (map mySpanId withSpans)
      startParent <- parentSpanIdOf (headOrFail "start" startSpans)
      stopParent <- parentSpanIdOf (headOrFail "stop" stopSpans)
      bodyParent <- parentSpanIdOf (headOrFail "body" bodySpans)

      startParent `shouldBe` Just withId
      stopParent `shouldBe` Just withId
      bodyParent `shouldBe` Just withId

    it "tags both stable and legacy database attributes" $ do
      spans <- captureSpans $ do
        result <-
          withTraced defaultEphemeralPgOtelConfig $ \_db -> pure ()
        result `shouldBe` Right ()

      let startSpans = findSpansByName "ephemeralpg.start" spans
      -- Stable (v1.27+) names.
      lookupTextAttribute "db.system.name" startSpans
        `shouldBe` Just "postgresql"
      lookupTextAttribute "db.namespace" startSpans
        `shouldBe` Just "postgres"
      -- Legacy names (the default Config builds the "postgres" database).
      lookupTextAttribute "db.system" startSpans
        `shouldBe` Just "postgresql"
      lookupTextAttribute "db.name" startSpans
        `shouldBe` Just "postgres"
      -- Library-specific attributes.
      let portAttr = lookupIntAttribute "ephemeralpg.port" startSpans
      portAttr `shouldSatisfy` maybe False (> 0)
      lookupTextAttribute "ephemeralpg.shutdown.mode" startSpans
        `shouldBe` Just "fast"

    it "marks spans Ok on the happy path" $ do
      spans <- captureSpans $ do
        result <-
          withTraced defaultEphemeralPgOtelConfig $ \_db -> pure ()
        result `shouldBe` Right ()

      spanStatusOf "ephemeralpg.with" spans `shouldBe` Just Ok
      spanStatusOf "ephemeralpg.start" spans `shouldBe` Just Ok
      spanStatusOf "ephemeralpg.stop" spans `shouldBe` Just Ok

-- | Initialise an in-memory tracer provider, run the action, shut the
-- provider down (which flushes spans), and return the captured list.
-- Spans are returned in completion order.
captureSpans :: IO () -> IO [ImmutableSpan]
captureSpans action = do
  (processor, listRef) <- inMemoryListExporter
  tp <- mkTracerProvider processor
  setGlobalTracerProvider tp
  action
  _ <- shutdownTracerProvider tp
  reverse <$> readIORef listRef

mkTracerProvider :: SpanProcessor -> IO TracerProvider
mkTracerProvider proc' =
  createTracerProvider
    [proc']
    emptyTracerProviderOptions
      { tracerProviderOptionsSampler = alwaysOn
      }

findSpansByName :: Text -> [ImmutableSpan] -> [ImmutableSpan]
findSpansByName n = filter ((== n) . spanName)

mySpanId :: ImmutableSpan -> SpanId
mySpanId = spanId . spanContext

parentSpanIdOf :: ImmutableSpan -> IO (Maybe SpanId)
parentSpanIdOf s = case spanParent s of
  Nothing -> pure Nothing
  Just p -> Just . spanId <$> getSpanContext p

spanStatusOf :: Text -> [ImmutableSpan] -> Maybe SpanStatus
spanStatusOf n spans = case findSpansByName n spans of
  [] -> Nothing
  (s : _) -> Just (spanStatus s)

lookupTextAttribute :: Text -> [ImmutableSpan] -> Maybe Text
lookupTextAttribute key = \case
  [] -> Nothing
  (s : _) -> case lookupAttribute (spanAttributes s) key of
    Just (AttributeValue (TextAttribute t)) -> Just t
    _ -> Nothing

lookupIntAttribute :: Text -> [ImmutableSpan] -> Maybe Int
lookupIntAttribute key = \case
  [] -> Nothing
  (s : _) -> case lookupAttribute (spanAttributes s) key of
    Just (AttributeValue (IntAttribute i)) -> Just (fromIntegral i)
    _ -> Nothing

headOrFail :: String -> [a] -> a
headOrFail _ (x : _) = x
headOrFail label _ = error ("expected at least one " <> label <> " span")
