-- | Programmatic tests for ephemeral-pg-opentelemetry.
--
-- Uses @hs-opentelemetry-exporter-in-memory@ to capture every emitted
-- 'ImmutableSpan' and asserts the resulting tree shape and attributes
-- without depending on stderr formatting.
module Main (main) where

import Data.IORef (readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import OpenTelemetry.Attributes
  ( Attribute (..),
    Attributes,
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
    SpanHot (..),
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
  setEnv "OTEL_SEMCONV_STABILITY_OPT_IN" "database/dup"
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

      let withId = snapshotSpanId (headOrFail "ephemeralpg.with" withSpans)

      snapshotParent (headOrFail "start" startSpans) `shouldBe` Just withId
      snapshotParent (headOrFail "stop" stopSpans) `shouldBe` Just withId
      snapshotParent (headOrFail "body" bodySpans) `shouldBe` Just withId

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

    it "tags both stable and legacy server attributes" $ do
      spans <- captureSpans $ do
        result <-
          withTraced defaultEphemeralPgOtelConfig $ \_db -> pure ()
        result `shouldBe` Right ()

      let startSpans = findSpansByName "ephemeralpg.start" spans
      -- The address is the Unix socket directory the instance listens
      -- on, so it is an absolute path rather than a hostname.
      let stableAddress = lookupTextAttribute "server.address" startSpans
      stableAddress `shouldSatisfy` maybe False (T.isPrefixOf "/")
      lookupTextAttribute "net.peer.name" startSpans
        `shouldBe` stableAddress
      -- Both families report the same port, and it agrees with the
      -- library-specific key.
      let stablePort = lookupIntAttribute "server.port" startSpans
      stablePort `shouldSatisfy` maybe False (> 0)
      lookupIntAttribute "net.peer.port" startSpans `shouldBe` stablePort
      lookupIntAttribute "ephemeralpg.port" startSpans `shouldBe` stablePort

    it "marks spans Ok on the happy path" $ do
      spans <- captureSpans $ do
        result <-
          withTraced defaultEphemeralPgOtelConfig $ \_db -> pure ()
        result `shouldBe` Right ()

      spanStatusOf "ephemeralpg.with" spans `shouldBe` Just Ok
      spanStatusOf "ephemeralpg.start" spans `shouldBe` Just Ok
      spanStatusOf "ephemeralpg.stop" spans `shouldBe` Just Ok

-- | A flattened, pure view of an exported span.
--
-- As of hs-opentelemetry 1.0 the mutable ("hot") span fields — name,
-- status, attributes — live behind an 'IORef' in 'spanHot' rather than
-- directly on 'ImmutableSpan', so read them once when the span is
-- captured and assert against plain values afterwards.
data SpanSnapshot = SpanSnapshot
  { snapshotName :: Text,
    snapshotStatus :: SpanStatus,
    snapshotAttributes :: Attributes,
    snapshotSpanId :: SpanId,
    snapshotParent :: Maybe SpanId
  }

-- | Initialise an in-memory tracer provider, run the action, shut the
-- provider down (which flushes spans), and return the captured list.
-- Spans are returned in completion order.
captureSpans :: IO () -> IO [SpanSnapshot]
captureSpans action = do
  (processor, listRef) <- inMemoryListExporter
  tp <- mkTracerProvider processor
  setGlobalTracerProvider tp
  action
  _ <- shutdownTracerProvider tp Nothing
  spans <- reverse <$> readIORef listRef
  traverse snapshotSpan spans

snapshotSpan :: ImmutableSpan -> IO SpanSnapshot
snapshotSpan s = do
  hot <- readIORef (spanHot s)
  parent <- traverse (fmap spanId . getSpanContext) (spanParent s)
  pure
    SpanSnapshot
      { snapshotName = hotName hot,
        snapshotStatus = hotStatus hot,
        snapshotAttributes = hotAttributes hot,
        snapshotSpanId = spanId (spanContext s),
        snapshotParent = parent
      }

mkTracerProvider :: SpanProcessor -> IO TracerProvider
mkTracerProvider proc' =
  createTracerProvider
    [proc']
    emptyTracerProviderOptions
      { tracerProviderOptionsSampler = alwaysOn
      }

findSpansByName :: Text -> [SpanSnapshot] -> [SpanSnapshot]
findSpansByName n = filter ((== n) . snapshotName)

spanStatusOf :: Text -> [SpanSnapshot] -> Maybe SpanStatus
spanStatusOf n spans = case findSpansByName n spans of
  [] -> Nothing
  (s : _) -> Just (snapshotStatus s)

lookupTextAttribute :: Text -> [SpanSnapshot] -> Maybe Text
lookupTextAttribute key = \case
  [] -> Nothing
  (s : _) -> case lookupAttribute (snapshotAttributes s) key of
    Just (AttributeValue (TextAttribute t)) -> Just t
    _ -> Nothing

lookupIntAttribute :: Text -> [SpanSnapshot] -> Maybe Int
lookupIntAttribute key = \case
  [] -> Nothing
  (s : _) -> case lookupAttribute (snapshotAttributes s) key of
    Just (AttributeValue (IntAttribute i)) -> Just (fromIntegral i)
    _ -> Nothing

headOrFail :: String -> [a] -> a
headOrFail _ (x : _) = x
headOrFail label _ = error ("expected at least one " <> label <> " span")
