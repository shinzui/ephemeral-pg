-- | Demo test-suite proving that ephemeral-pg-opentelemetry's lifecycle
-- spans stitch under a parent test span produced by
-- @hs-opentelemetry-instrumentation-hspec@.
--
-- Running @cabal test ephemeral-pg-opentelemetry-demo@ writes a span
-- record per emitted span to stderr, with @parent=<spanId>@ visible so
-- parent/child relationships can be verified by eye. The default
-- formatter from @hs-opentelemetry-exporter-handle@ does not print
-- parent IDs, so this file uses a small custom formatter — see
-- 'parentAwareFormatter'.
module Main (main) where

import Control.Monad (void)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import OpenTelemetry.Context (empty)
import OpenTelemetry.Context.ThreadLocal (attachContext, getContext)
import OpenTelemetry.Exporter.Handle.Span (stderrExporter')
import OpenTelemetry.Instrumentation.EphemeralPg
  ( defaultEphemeralPgOtelConfig,
    withTraced,
  )
import OpenTelemetry.Instrumentation.Hspec (instrumentSpec)
import OpenTelemetry.Processor.Batch.Span (batchProcessor, batchTimeoutConfig)
import OpenTelemetry.Trace
  ( ImmutableSpan (..),
    Span,
    SpanArguments,
    SpanContext (..),
    SpanStatus (..),
    TracerProvider,
    createTracerProvider,
    defaultSpanArguments,
    getGlobalTracerProvider,
    getTracerProviderInitializationOptions,
    makeTracer,
    setGlobalTracerProvider,
    shutdownTracerProvider,
    tracerOptions,
    tracerProviderOptionsSampler,
  )
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Core (getSpanContext)
import OpenTelemetry.Trace.Id (Base (..), spanIdBaseEncodedText, traceIdBaseEncodedText)
import OpenTelemetry.Trace.Sampler (alwaysOn)
import Test.Hspec
import Test.Hspec.Runner (defaultConfig, hspecWith)
import UnliftIO (MonadUnliftIO, bracket)

main :: IO ()
main = do
  void $ attachContext empty
  bracket initializeTracing shutdownTracerProvider $ \_ ->
    inSpan "Run tests" defaultSpanArguments runTests

runTests :: IO ()
runTests = do
  tp <- getGlobalTracerProvider
  let tracer = makeTracer tp "ephemeral-pg-opentelemetry-demo" tracerOptions
  ctxt <- getContext
  hspecWith defaultConfig $
    instrumentSpec tracer ctxt $
      describe "ephemeral-pg under OpenTelemetry" $
        it "emits a span tree under withTraced" $ do
          result <- withTraced defaultEphemeralPgOtelConfig $ \_db -> pure ()
          case result of
            Left err -> expectationFailure ("withTraced failed: " <> show err)
            Right () -> pure ()

initializeTracing :: IO TracerProvider
initializeTracing = do
  (processors, opts) <- getTracerProviderInitializationOptions
  stderrProc <-
    batchProcessor batchTimeoutConfig (stderrExporter' parentAwareFormatter)
  provider <-
    createTracerProvider
      (stderrProc : processors)
      (opts {tracerProviderOptionsSampler = alwaysOn})
  setGlobalTracerProvider provider
  pure provider

inSpan :: (MonadUnliftIO m) => T.Text -> SpanArguments -> m a -> m a
inSpan name args act = do
  tp <- getGlobalTracerProvider
  let tracer = makeTracer tp "ephemeral-pg-opentelemetry-demo" tracerOptions
  Trace.inSpan tracer name args act

-- | Custom formatter that prints @trace=<id> span=<id> parent=<id>
-- status=<status> name=<name>@. The default formatter omits parent
-- IDs, which makes parent/child relationships invisible in stderr.
parentAwareFormatter :: ImmutableSpan -> IO L.Text
parentAwareFormatter sp = do
  let ctx = spanContext sp
      traceIdText = traceIdBaseEncodedText Base16 (traceId ctx)
      spanIdText = spanIdBaseEncodedText Base16 (spanId ctx)
  parentText <- renderParent (spanParent sp)
  pure $
    L.concat
      [ "trace=",
        L.fromStrict traceIdText,
        " span=",
        L.fromStrict spanIdText,
        " parent=",
        L.fromStrict parentText,
        " status=",
        L.fromStrict (renderStatus (spanStatus sp)),
        " name=",
        L.fromStrict (spanName sp)
      ]

renderParent :: Maybe Span -> IO T.Text
renderParent Nothing = pure "ROOT"
renderParent (Just s) = do
  ctx <- getSpanContext s
  pure $ spanIdBaseEncodedText Base16 (spanId ctx)

renderStatus :: SpanStatus -> T.Text
renderStatus = \case
  Unset -> "unset"
  Ok -> "ok"
  Error msg -> "error:" <> msg
