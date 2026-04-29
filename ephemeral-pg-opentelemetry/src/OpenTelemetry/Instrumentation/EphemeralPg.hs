{-# LANGUAGE TemplateHaskell #-}

-- | OpenTelemetry tracing wrappers around the ephemeral-pg lifecycle.
--
-- Each wrapper emits one span named after its public counterpart in
-- @EphemeralPg@:
--
-- * 'withTraced'           -> @ephemeralpg.with@, with @ephemeralpg.start@,
--                             @ephemeralpg.with.body@, and @ephemeralpg.stop@
--                             as children.
-- * 'withCachedTraced'     -> @ephemeralpg.with_cached@ (analogous shape).
-- * 'startTraced'          -> @ephemeralpg.start@.
-- * 'stopTraced'           -> @ephemeralpg.stop@.
-- * 'restartTraced'        -> @ephemeralpg.restart@.
-- * 'createSnapshotTraced' -> @ephemeralpg.snapshot.create@.
-- * 'restoreSnapshotTraced'-> @ephemeralpg.snapshot.restore@.
-- * 'deleteSnapshotTraced' -> @ephemeralpg.snapshot.delete@.
-- * 'dumpTraced'           -> @ephemeralpg.dump@.
-- * 'restoreTraced'        -> @ephemeralpg.restore@.
--
-- These are intended to be invoked from inside a test that is itself
-- already running under a parent span — for example, an @it@ block
-- instrumented by @hs-opentelemetry-instrumentation-hspec@. The result is
-- a trace in which a setup-heavy test makes its phases observable.
--
-- = Semantic conventions
--
-- Spans that have a 'Pg.Database' in scope receive standard database
-- attributes. The exact attribute names obey
-- @OTEL_SEMCONV_STABILITY_OPT_IN@:
--
-- * Stable (v1.27+): @db.system.name@, @db.namespace@.
-- * Legacy:          @db.system@, @db.name@.
-- * Both:             stable + legacy.
--
-- Plus library-specific keys: @ephemeralpg.port@,
-- @ephemeralpg.shutdown.mode@.
--
-- = Error recording
--
-- Errors are recorded uniformly:
--
-- * @error.type@ is set to the error's constructor name (typed errors)
--   or a stable string (for @Either Text@ returns).
-- * @setStatus (Error msg)@ is called with the rendered or raw message.
-- * 'recordException' is called for typed errors that derive 'Exception'
--   (i.e. 'Pg.StartError').
module OpenTelemetry.Instrumentation.EphemeralPg
  ( -- * Configuration
    EphemeralPgOtelConfig (..),
    defaultEphemeralPgOtelConfig,

    -- * Tracer
    ephemeralPgTracer,

    -- * Lifecycle wrappers
    withTraced,
    withCachedTraced,
    startTraced,
    stopTraced,
    restartTraced,

    -- * Snapshot wrappers
    createSnapshotTraced,
    restoreSnapshotTraced,
    deleteSnapshotTraced,

    -- * Dump and restore wrappers
    dumpTraced,
    restoreTraced,
  )
where

import Control.Exception (mask, onException)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as T
import EphemeralPg qualified as Pg
import EphemeralPg.Dump qualified as Dump
import EphemeralPg.Snapshot qualified as Snapshot
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))
import OpenTelemetry.SemanticsConfig
  ( HttpOption (..),
    getSemanticsOptions,
    httpOption,
  )
import OpenTelemetry.Trace.Core
  ( Span,
    SpanStatus (..),
    Tracer,
    addAttribute,
    addAttributes,
    defaultSpanArguments,
    detectInstrumentationLibrary,
    getGlobalTracerProvider,
    inSpan',
    makeTracer,
    recordException,
    setStatus,
    tracerOptions,
  )

-- | Tunable configuration for the @ephemeral-pg-opentelemetry@ wrappers.
--
-- Mirrors the shape of @HasqlConfig@ from @hasql-opentelemetry@.
-- Attributes set in 'extraAttributes' are attached to every span the
-- library emits.
data EphemeralPgOtelConfig = EphemeralPgOtelConfig
  { -- | Free-form label appended as @ephemeralpg.service_label@. Empty
    -- means "do not attach".
    serviceLabel :: !Text,
    -- | Extra attributes attached to every span.
    extraAttributes :: !(HashMap Text Attribute)
  }

defaultEphemeralPgOtelConfig :: EphemeralPgOtelConfig
defaultEphemeralPgOtelConfig =
  EphemeralPgOtelConfig
    { serviceLabel = "",
      extraAttributes = HashMap.empty
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
-- * @ephemeralpg.start@ — covers @initdb@, @postgres@, @pg_isready@,
--                         and @createdb@. Internal sub-phases are
--                         opaque until a @Phase@ hook is added to the
--                         core library.
-- * @ephemeralpg.with.body@ — covers the user action.
-- * @ephemeralpg.stop@ — covers @postgres@ shutdown and tempdir
--                         cleanup.
--
-- This wrapper re-implements 'Pg.with' rather than wrapping it so that
-- the start and stop phases each become visible spans. The @mask@ /
-- @onException@ pattern is preserved verbatim so cleanup safety is
-- identical to the core library.
withTraced ::
  EphemeralPgOtelConfig ->
  (Pg.Database -> IO a) ->
  IO (Either Pg.StartError a)
withTraced = withTracedNamed "ephemeralpg.with" Pg.start

-- | Like 'withTraced' but uses 'Pg.startCached' / 'Pg.stop' for faster
-- repeated runs. The parent span is named @ephemeralpg.with_cached@.
withCachedTraced ::
  EphemeralPgOtelConfig ->
  (Pg.Database -> IO a) ->
  IO (Either Pg.StartError a)
withCachedTraced =
  withTracedNamed
    "ephemeralpg.with_cached"
    (\cfg -> Pg.startCached cfg Pg.defaultCacheConfig)

-- Shared body for 'withTraced' and 'withCachedTraced'. Takes a starter
-- function so we can swap in 'Pg.start' or 'Pg.startCached'.
withTracedNamed ::
  Text ->
  (Pg.Config -> IO (Either Pg.StartError Pg.Database)) ->
  EphemeralPgOtelConfig ->
  (Pg.Database -> IO a) ->
  IO (Either Pg.StartError a)
withTracedNamed spanName starter cfg action = do
  tracer <- ephemeralPgTracer
  inSpan' tracer spanName defaultSpanArguments $ \sp -> do
    tagCommon sp cfg
    mask \restore -> do
      startResult <- startTracedWith tracer cfg starter
      case startResult of
        Left err -> do
          recordStartError sp err
          pure (Left err)
        Right db -> do
          attachDbAttributes sp db
          a <-
            inSpan' tracer "ephemeralpg.with.body" defaultSpanArguments \_ ->
              restore (action db)
                `onException` stopTraced' tracer cfg db
          stopTraced' tracer cfg db
          setStatus sp Ok
          pure (Right a)

-- | Wrap 'EphemeralPg.start' in an @ephemeralpg.start@ span. On
-- 'Left', sets span status to @Error@ and attaches @error.type@ plus a
-- 'recordException' event.
startTraced ::
  EphemeralPgOtelConfig ->
  Pg.Config ->
  IO (Either Pg.StartError Pg.Database)
startTraced cfg pgConfig = do
  tracer <- ephemeralPgTracer
  startTracedWith tracer cfg (\_ -> Pg.start pgConfig)

startTracedWith ::
  Tracer ->
  EphemeralPgOtelConfig ->
  (Pg.Config -> IO (Either Pg.StartError Pg.Database)) ->
  IO (Either Pg.StartError Pg.Database)
startTracedWith tracer cfg starter =
  inSpan' tracer "ephemeralpg.start" defaultSpanArguments \sp -> do
    tagCommon sp cfg
    result <- starter Pg.defaultConfig
    case result of
      Left err -> recordStartError sp err
      Right db -> do
        attachDbAttributes sp db
        setStatus sp Ok
    pure result

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
    tagCommon sp cfg
    attachDbAttributes sp db
    result <- Pg.restart db
    case result of
      Left err -> recordStartError sp err
      Right db' -> do
        attachDbAttributes sp db'
        setStatus sp Ok
    pure result

-- | Wrap 'EphemeralPg.Snapshot.createSnapshot' in an
-- @ephemeralpg.snapshot.create@ span.
createSnapshotTraced ::
  EphemeralPgOtelConfig ->
  Pg.Database ->
  IO (Either Text Snapshot.Snapshot)
createSnapshotTraced cfg db = do
  tracer <- ephemeralPgTracer
  inSpan' tracer "ephemeralpg.snapshot.create" defaultSpanArguments \sp -> do
    tagCommon sp cfg
    attachDbAttributes sp db
    result <- Snapshot.createSnapshot db
    finishWithTextResult sp "SnapshotCreateFailure" result

-- | Wrap 'EphemeralPg.Snapshot.restoreSnapshot' in an
-- @ephemeralpg.snapshot.restore@ span.
restoreSnapshotTraced ::
  EphemeralPgOtelConfig ->
  Snapshot.Snapshot ->
  Pg.Database ->
  IO (Either Text ())
restoreSnapshotTraced cfg snap db = do
  tracer <- ephemeralPgTracer
  inSpan' tracer "ephemeralpg.snapshot.restore" defaultSpanArguments \sp -> do
    tagCommon sp cfg
    attachDbAttributes sp db
    result <- Snapshot.restoreSnapshot snap db
    finishWithTextResult sp "SnapshotRestoreFailure" result

-- | Wrap 'EphemeralPg.Snapshot.deleteSnapshot' in an
-- @ephemeralpg.snapshot.delete@ span.
deleteSnapshotTraced ::
  EphemeralPgOtelConfig ->
  Snapshot.Snapshot ->
  IO ()
deleteSnapshotTraced cfg snap = do
  tracer <- ephemeralPgTracer
  inSpan' tracer "ephemeralpg.snapshot.delete" defaultSpanArguments \sp -> do
    tagCommon sp cfg
    Snapshot.deleteSnapshot snap
    setStatus sp Ok

-- | Wrap 'EphemeralPg.Dump.dump' in an @ephemeralpg.dump@ span.
dumpTraced ::
  EphemeralPgOtelConfig ->
  Pg.Database ->
  FilePath ->
  Dump.DumpOptions ->
  IO (Either Text ())
dumpTraced cfg db outPath opts = do
  tracer <- ephemeralPgTracer
  inSpan' tracer "ephemeralpg.dump" defaultSpanArguments \sp -> do
    tagCommon sp cfg
    attachDbAttributes sp db
    result <- Dump.dump db outPath opts
    finishWithTextResult sp "DumpFailure" result

-- | Wrap 'EphemeralPg.Dump.restore' in an @ephemeralpg.restore@ span.
restoreTraced ::
  EphemeralPgOtelConfig ->
  Pg.Database ->
  FilePath ->
  IO (Either Text ())
restoreTraced cfg db inPath = do
  tracer <- ephemeralPgTracer
  inSpan' tracer "ephemeralpg.restore" defaultSpanArguments \sp -> do
    tagCommon sp cfg
    attachDbAttributes sp db
    result <- Dump.restore db inPath
    finishWithTextResult sp "RestoreFailure" result

-- Internal helpers shared between the public wrappers.

stopTraced' :: Tracer -> EphemeralPgOtelConfig -> Pg.Database -> IO ()
stopTraced' tracer cfg db =
  inSpan' tracer "ephemeralpg.stop" defaultSpanArguments \sp -> do
    tagCommon sp cfg
    attachDbAttributes sp db
    Pg.stop db
    setStatus sp Ok

-- | Attach the common attributes (extraAttributes plus serviceLabel)
-- to a span. These are independent of the database value.
tagCommon :: Span -> EphemeralPgOtelConfig -> IO ()
tagCommon sp cfg = do
  addAttributes sp (extraAttributes cfg)
  if T.null (serviceLabel cfg)
    then pure ()
    else
      addAttribute sp ("ephemeralpg.service_label" :: Text) (serviceLabel cfg)

-- | Attach attributes derived from a 'Pg.Database': the
-- @db.system.name@/@db.namespace@ family (with stability honour) plus
-- @ephemeralpg.port@ and @ephemeralpg.shutdown.mode@.
attachDbAttributes :: Span -> Pg.Database -> IO ()
attachDbAttributes sp db = do
  attrs <- dbAttributes db
  addAttributes sp (HashMap.fromList attrs)

dbAttributes :: Pg.Database -> IO [(Text, Attribute)]
dbAttributes db = do
  opt <- httpOption <$> getSemanticsOptions
  let pgName = "postgresql" :: Text
      dbNamespace = db.databaseName
      port = fromIntegral db.port :: Int
      shutdownModeText = renderShutdownMode db.shutdownMode
      stable =
        [ ("db.system.name", toAttribute pgName),
          ("db.namespace", toAttribute dbNamespace)
        ]
      legacy =
        [ ("db.system", toAttribute pgName),
          ("db.name", toAttribute dbNamespace)
        ]
      ephemeralPg =
        [ ("ephemeralpg.port", toAttribute port),
          ("ephemeralpg.shutdown.mode", toAttribute shutdownModeText)
        ]
  pure $ case opt of
    Stable -> stable <> ephemeralPg
    Old -> legacy <> ephemeralPg
    StableAndOld -> stable <> legacy <> ephemeralPg

renderShutdownMode :: Pg.ShutdownMode -> Text
renderShutdownMode = \case
  Pg.ShutdownGraceful -> "graceful"
  Pg.ShutdownFast -> "fast"
  Pg.ShutdownImmediate -> "immediate"

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

-- | Record a 'Pg.StartError' on a span: @error.type@ + @setStatus
-- Error@ + @recordException@. 'Pg.StartError' derives 'Exception', so
-- it can be passed to 'recordException' directly.
recordStartError :: Span -> Pg.StartError -> IO ()
recordStartError sp err = do
  addAttribute sp ("error.type" :: Text) (startErrorType err)
  recordException sp HashMap.empty Nothing err
  setStatus sp (Error (Pg.renderStartError err))

-- | Finish a span whose action returned @Either Text r@. On 'Left',
-- attaches @error.type@ (a fixed string supplied by the caller) and
-- sets span status to @Error@. On 'Right', sets status to @Ok@.
-- 'Text' is not an 'Exception', so we do not call 'recordException'.
finishWithTextResult :: Span -> Text -> Either Text r -> IO (Either Text r)
finishWithTextResult sp errType result = do
  case result of
    Left msg -> do
      addAttribute sp ("error.type" :: Text) errType
      setStatus sp (Error msg)
    Right _ -> setStatus sp Ok
  pure result
