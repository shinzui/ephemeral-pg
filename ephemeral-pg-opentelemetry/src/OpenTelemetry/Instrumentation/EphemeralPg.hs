{-# LANGUAGE TemplateHaskell #-}

-- | OpenTelemetry tracing wrappers around the ephemeral-pg lifecycle.
--
-- Each wrapper emits one span named after its public counterpart in
-- @EphemeralPg@:
--
-- * 'withTraced'    -> @ephemeralpg.with@, with @ephemeralpg.start@,
--                      @ephemeralpg.with.body@, and @ephemeralpg.stop@ as
--                      children.
-- * 'startTraced'   -> @ephemeralpg.start@.
-- * 'stopTraced'    -> @ephemeralpg.stop@.
-- * 'restartTraced' -> @ephemeralpg.restart@.
--
-- These are intended to be invoked from inside a test that is itself
-- already running under a parent span — for example, an @it@ block
-- instrumented by @hs-opentelemetry-instrumentation-hspec@. The result is
-- a trace in which a setup-heavy test makes its phases observable.
--
-- Errors of type 'EphemeralPg.StartError' are recorded uniformly: an
-- @error.type@ attribute carrying the constructor name plus a
-- 'SpanStatus.Error' with the rendered message.
module OpenTelemetry.Instrumentation.EphemeralPg
  ( -- * Configuration
    EphemeralPgOtelConfig (..),
    defaultEphemeralPgOtelConfig,

    -- * Tracer
    ephemeralPgTracer,

    -- * Lifecycle wrappers
    withTraced,
    startTraced,
    stopTraced,
    restartTraced,
  )
where

import Control.Exception (mask, onException)
import Data.Text (Text)
import Data.Text qualified as T
import EphemeralPg qualified as Pg
import OpenTelemetry.Trace.Core
  ( Span,
    SpanStatus (..),
    Tracer,
    addAttribute,
    defaultSpanArguments,
    detectInstrumentationLibrary,
    getGlobalTracerProvider,
    inSpan',
    makeTracer,
    setStatus,
    tracerOptions,
  )

-- | Tunable configuration for the @ephemeral-pg-opentelemetry@ wrappers.
--
-- Mirrors the shape of @HasqlConfig@ from @hasql-opentelemetry@. For the
-- M2 spike the only field is a free-form 'serviceLabel' attribute that
-- callers can use to tag all spans produced by a particular suite or
-- harness; this is intentionally minimal and will grow in M3.
newtype EphemeralPgOtelConfig = EphemeralPgOtelConfig
  { serviceLabel :: Text
  }

defaultEphemeralPgOtelConfig :: EphemeralPgOtelConfig
defaultEphemeralPgOtelConfig =
  EphemeralPgOtelConfig
    { serviceLabel = ""
    }

-- | Tracer for this instrumentation library. Wired to the global tracer
-- provider; the package name and version are injected at compile time
-- via the 'detectInstrumentationLibrary' Template Haskell splice.
ephemeralPgTracer :: IO Tracer
ephemeralPgTracer = do
  tp <- getGlobalTracerProvider
  pure $ makeTracer tp $detectInstrumentationLibrary tracerOptions

-- | Run @action@ against a temporary database, with the entire
-- start/use/stop cycle observed under an @ephemeralpg.with@ span.
--
-- Children of the resulting span are:
--
-- * @ephemeralpg.start@   — covers @initdb@, @postgres@, @pg_isready@,
--                           and @createdb@. Internal sub-phases are
--                           opaque in M2 and will be exposed in M3.
-- * @ephemeralpg.with.body@ — covers the user action.
-- * @ephemeralpg.stop@    — covers @postgres@ shutdown and tempdir
--                           cleanup.
--
-- This wrapper re-implements 'Pg.with' rather than wrapping it so that
-- the start and stop phases each become visible spans. The @mask@/
-- @onException@ pattern is preserved verbatim so cleanup safety is
-- identical to the core library.
withTraced ::
  EphemeralPgOtelConfig ->
  (Pg.Database -> IO a) ->
  IO (Either Pg.StartError a)
withTraced cfg action = do
  tracer <- ephemeralPgTracer
  inSpan' tracer "ephemeralpg.with" defaultSpanArguments $ \sp ->
    tagServiceLabel sp cfg >> mask \restore -> do
      startResult <- startTraced' tracer cfg Pg.defaultConfig
      case startResult of
        Left err -> do
          recordStartError sp err
          pure (Left err)
        Right db -> do
          a <-
            inSpan' tracer "ephemeralpg.with.body" defaultSpanArguments \_ ->
              restore (action db)
                `onException` stopTraced' tracer cfg db
          stopTraced' tracer cfg db
          setStatus sp Ok
          pure (Right a)

-- | Wrap 'EphemeralPg.start' in an @ephemeralpg.start@ span. On
-- 'Left', sets span status to @Error@ and attaches @error.type@.
startTraced ::
  EphemeralPgOtelConfig ->
  Pg.Config ->
  IO (Either Pg.StartError Pg.Database)
startTraced cfg pgConfig = do
  tracer <- ephemeralPgTracer
  startTraced' tracer cfg pgConfig

-- | Wrap 'EphemeralPg.stop' in an @ephemeralpg.stop@ span.
stopTraced ::
  EphemeralPgOtelConfig ->
  Pg.Database ->
  IO ()
stopTraced cfg db = do
  tracer <- ephemeralPgTracer
  stopTraced' tracer cfg db

-- | Wrap 'EphemeralPg.restart' in an @ephemeralpg.restart@ span. On
-- 'Left', sets span status to @Error@ and attaches @error.type@.
restartTraced ::
  EphemeralPgOtelConfig ->
  Pg.Database ->
  IO (Either Pg.StartError Pg.Database)
restartTraced cfg db = do
  tracer <- ephemeralPgTracer
  inSpan' tracer "ephemeralpg.restart" defaultSpanArguments \sp -> do
    tagServiceLabel sp cfg
    result <- Pg.restart db
    case result of
      Left err -> recordStartError sp err
      Right _ -> setStatus sp Ok
    pure result

-- Internal helpers shared between 'withTraced' and the standalone
-- wrappers. They take an explicit 'Tracer' so we only construct it
-- once per top-level call.

startTraced' ::
  Tracer ->
  EphemeralPgOtelConfig ->
  Pg.Config ->
  IO (Either Pg.StartError Pg.Database)
startTraced' tracer cfg pgConfig =
  inSpan' tracer "ephemeralpg.start" defaultSpanArguments \sp -> do
    tagServiceLabel sp cfg
    result <- Pg.start pgConfig
    case result of
      Left err -> recordStartError sp err
      Right _ -> setStatus sp Ok
    pure result

stopTraced' :: Tracer -> EphemeralPgOtelConfig -> Pg.Database -> IO ()
stopTraced' tracer cfg db =
  inSpan' tracer "ephemeralpg.stop" defaultSpanArguments \sp -> do
    tagServiceLabel sp cfg
    Pg.stop db
    setStatus sp Ok

tagServiceLabel :: Span -> EphemeralPgOtelConfig -> IO ()
tagServiceLabel sp cfg
  | T.null (serviceLabel cfg) = pure ()
  | otherwise = addAttribute sp ("ephemeralpg.service_label" :: Text) (serviceLabel cfg)

-- | Constructor name for a 'Pg.StartError', used as the conventional
-- @error.type@ attribute (low cardinality).
startErrorType :: Pg.StartError -> Text
startErrorType = \case
  Pg.InitDbError {} -> "InitDbError"
  Pg.PostgresStartError {} -> "PostgresStartError"
  Pg.CreateDbError {} -> "CreateDbError"
  Pg.ConfigError {} -> "ConfigError"
  Pg.ResourceError {} -> "ResourceError"
  Pg.TimeoutError {} -> "TimeoutError"

recordStartError :: Span -> Pg.StartError -> IO ()
recordStartError sp err = do
  addAttribute sp ("error.type" :: Text) (startErrorType err)
  setStatus sp (Error (Pg.renderStartError err))
