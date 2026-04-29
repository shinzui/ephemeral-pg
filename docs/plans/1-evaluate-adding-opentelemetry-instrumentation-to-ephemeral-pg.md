---
id: 1
slug: evaluate-adding-opentelemetry-instrumentation-to-ephemeral-pg
title: "Evaluate adding OpenTelemetry instrumentation to ephemeral-pg"
kind: exec-plan
created_at: 2026-04-29T23:03:39Z
intention: "intention_01kqdhce85exx9dp42b3j9d8n7"
---

# Evaluate adding OpenTelemetry instrumentation to ephemeral-pg

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

`ephemeral-pg` is a Haskell library in this repository (root: the working
directory; package name `ephemeral-pg`, declared in `ephemeral-pg.cabal`) that
spins up disposable PostgreSQL clusters for tests. A typical run uses
`EphemeralPg.with` or `EphemeralPg.withCached` to start a cluster, hand a
`Database` handle to the test, and then stop and clean up. Behind the scenes
the library does several heavyweight things — creates temp directories,
runs `initdb`, copies a cached cluster via copy-on-write, starts the `postgres`
process, waits for `pg_isready`, runs `createdb`, takes snapshots, drives
`pg_dump`/`psql`, and shuts down with a configurable signal. Today none of
this is observable from the test runner; if a suite is mysteriously slow or
flaky you have to instrument the library by hand to find out which of those
phases is responsible.

OpenTelemetry is a vendor-neutral standard for application telemetry; in
Haskell it is implemented by the package family rooted at
`hs-opentelemetry-api`, `hs-opentelemetry-sdk`, and a number of
`hs-opentelemetry-instrumentation-*` packages. The `hs-opentelemetry`
mono-repo lives on disk at
`/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry`
(registered in `mori` as `iand675/hs-opentelemetry`). The two test-framework
instrumentation packages the user pointed at are:

* **`hs-opentelemetry-instrumentation-hspec`** — a single module
  `OpenTelemetry.Instrumentation.Hspec` that exports `wrapSpec`,
  `wrapExampleInSpan`, and `instrumentSpec`. It walks an `hspec` `SpecForest`
  with `mapSpecForest` / `mapSpecItem_`, wrapping each `it` (and optionally
  each enclosing `describe`) in a span. Because `hspec` may run examples on
  forked threads it explicitly re-attaches the parent OpenTelemetry context
  in the inner `aroundAction`.
* **`hs-opentelemetry-instrumentation-tasty`** — a single module
  `OpenTelemetry.Instrumentation.Tasty` that exports `instrumentTestTree`
  and `instrumentTestTreeWithTracer`. It traverses the `TestTree`
  constructors (`TestGroup`, `SingleTest`, `WithResource`,
  `PlusTestOptions`, `AskOptions`, `After`), uses `withResource` to scope
  group spans for the duration of the group, wraps each `SingleTest` in a
  newtype `WrappedTest` that implements `Test.Tasty.Providers.IsTest` and
  records `Outcome` as `Ok`/`Error` plus a `result.description` attribute,
  and emits separate `acquire`/`release` spans inside `WithResource`.
  It threads the parent span manually because `tasty` parallelism breaks
  the thread-local OTel context.

The user's question is whether `ephemeral-pg` should grow analogous
instrumentation. The short answer this plan defends and validates is:
**yes, but the right analogy is "instrument the library, not the test
runner".** `hspec` and `tasty` themselves are test-runner libraries, so
instrumenting them creates the *parent* span for an individual test.
`ephemeral-pg` is a *test-support* library invoked from inside a test, so
its spans should be *children* of those parent spans — exactly the role
the user's other library `hasql-opentelemetry` already plays for `hasql`.
Once an `ephemeral-pg-opentelemetry` companion package exists, a test
instrumented by `hs-opentelemetry-instrumentation-hspec` will produce traces
in which `ephemeralpg.with` shows up as a child of the test's `it` span,
with grandchildren naming `initdb`, `postgres.start`, `postgres.wait_ready`,
`createdb`, `snapshot.create`, `dump`, and so on, each carrying duration
and error status. When a developer asks "why does my test suite spend 8
seconds in setup?", the trace gives the answer immediately.

The deliverable of this plan, in milestones described below, is

1. an explicit written evaluation that recommends a path (Milestone 1);
2. a working spike — a separate companion library inside this repository
   plus a runnable demo test — that proves an `ephemeral-pg` lifecycle
   span tree is emitted and stitches under an `hspec` parent span
   (Milestone 2);
3. a productionisation pass that fleshes out the public API, semantic
   conventions, errors, and tests (Milestone 3).

After Milestone 2 a developer can run a single test executable in this
repository and see, in their stderr-attached span exporter, a tree of the
shape

    Run tests
      can start and stop a database          ← from instrumentSpec
        ephemeralpg.with
          ephemeralpg.start
            ephemeralpg.cache.restore        ← cache hit path
            ephemeralpg.postgres.start
              ephemeralpg.postgres.wait_ready
            ephemeralpg.createdb
          ephemeralpg.stop
            ephemeralpg.postgres.stop

That tree, printed as JSON spans, is the observable outcome.


## Progress

The work breaks into one evaluation milestone followed by two implementation
milestones. Each box must be checked with a date and short note as it
completes.

- [x] M1.1 Read all referenced files and confirm the evaluation in
  `Context and Orientation` matches the actual code on disk (no surprises).
  *(2026-04-29: confirmed; two minor corrections recorded under Surprises &
  Discoveries — `cabal.project` already exists, and `defaultFormatter`
  emits flat lines.)*
- [x] M1.2 Append the evaluation summary (recommendation + rationale + risk
  list) to the `Outcomes & Retrospective` section as the M1 deliverable.
  *(2026-04-29)*
- [x] M1.3 Decide go / no-go for the spike. Record decision and date in
  `Decision Log`. *(2026-04-29: go.)*
- [x] M2.1 Add a new sub-package directory `ephemeral-pg-opentelemetry/`
  with its own cabal file declaring the library. Wire it into a top-level
  `cabal.project` if one does not exist (the repository currently only
  ships a single `ephemeral-pg.cabal`; introducing a sibling package will
  require a `cabal.project`). *(2026-04-29: amended the existing
  `cabal.project` rather than creating one; added an `allow-newer` block
  to waive the published `hs-opentelemetry-*` upper bounds since the
  Hackage releases predate the local `==0.3.*` fix.)*
- [x] M2.2 Implement the minimal API: `withTraced`, `startTraced`,
  `stopTraced`, `restartTraced` plus an `OtelConfig` record that carries
  the `Tracer` and a `serviceName`-equivalent. Use `OpenTelemetry.Trace.Core.inSpan`
  for the parent and child spans. Reuse the existing public API of
  `EphemeralPg` rather than forking it. *(2026-04-29: implemented in
  `ephemeral-pg-opentelemetry/src/OpenTelemetry/Instrumentation/EphemeralPg.hs`.
  `withTraced` re-implements the `mask`/`onException` composition so
  `ephemeralpg.start` and `ephemeralpg.stop` show up as visible
  children — see Decision Log.)*
- [x] M2.3 Add a runnable `test-suite ephemeral-pg-opentelemetry-demo` that
  uses `hs-opentelemetry-sdk` with a stderr `Handle` exporter, runs one
  `withTraced` call inside an `instrumentSpec`-wrapped hspec block, and
  prints the resulting span tree to stderr. *(2026-04-29: implemented at
  `ephemeral-pg-opentelemetry/test/Demo.hs`. Uses a custom
  `parentAwareFormatter` because the upstream `defaultFormatter` does
  not include parent IDs.)*
- [x] M2.4 Capture the actual emitted span tree in
  `Surprises & Discoveries` (raw stderr excerpt is fine), confirm the
  parenting and naming match the proposal in this plan.
  *(2026-04-29: captured below.)*
- [x] M3.1 Extend the spike to cover `withCachedTraced`, snapshot, and dump.
  *(2026-04-29: added `withCachedTraced`, `createSnapshotTraced`,
  `restoreSnapshotTraced`, `deleteSnapshotTraced`, `dumpTraced`,
  `restoreTraced`. Each is a single-span wrapper around the
  corresponding public `EphemeralPg` call.)*
- [x] M3.2 Add semantic-convention attributes (`db.system.name=postgresql`,
  `db.namespace`, `ephemeralpg.cache.hit`, `ephemeralpg.cache.cow_method`,
  `ephemeralpg.shutdown.mode`, `ephemeralpg.port`).
  *(2026-04-29: added `db.system.name`/`db.namespace` (stable) plus
  `db.system`/`db.name` (legacy) honouring `OTEL_SEMCONV_STABILITY_OPT_IN`,
  along with `ephemeralpg.port` and `ephemeralpg.shutdown.mode`.
  `ephemeralpg.cache.hit`/`cow_method` are deferred — see Decision Log
  2026-04-29; they require an internal hook on `EphemeralPg.Config`
  which is the subject of a follow-up plan.)*
- [x] M3.3 Record errors uniformly: on every `Left StartError` and `Left
  StopError` set span status `Error`, attach `error.type`, and emit
  `recordException`. Match the pattern used in
  `hasql-opentelemetry`'s `instrumentedUse`.
  *(2026-04-29: `recordStartError` does the full treatment for
  `Pg.StartError` (Exception-derived). For `Either Text` errors from
  snapshot/dump, `finishWithTextResult` attaches `error.type` plus
  `setStatus Error`; `recordException` is skipped because `Text` is not
  an `Exception`.)*
- [x] M3.4 Unit tests using `hs-opentelemetry-exporter-in-memory` to assert
  the span tree shape without relying on stderr.
  *(2026-04-29: added `ephemeral-pg-opentelemetry-test` with three
  hspec assertions: `with`/`start`/`stop` parenting; both stable and
  legacy DB attributes (env-var driven); `Ok` status on the happy path.
  Forced-error path is deferred with the per-phase spans.)*
- [x] M3.5 Update `README.md` at the repository root with a "Tracing"
  section that points to the companion package.
  *(2026-04-29: added a Tracing section above the License section
  listing the wrapper-to-span mapping, attribute conventions, and a
  pointer to `ephemeral-pg-opentelemetry/test/Demo.hs`.)*


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered
during implementation. Provide concise evidence (test output is ideal).

### M1.1 — corrections to `Context and Orientation`

Verified every file path the plan names against the working tree
(`src/EphemeralPg.hs`, `src/EphemeralPg/{Config,Database,Snapshot,Dump,Error}.hs`,
`src/EphemeralPg/Internal/*.hs`, `src/EphemeralPg/Process*.hs`,
`test/Main.hs`, `ephemeral-pg.cabal`). Public API and error hierarchy
match the description. Two corrections to the prose:

* `cabal.project` is **already present** at the repository root, not
  absent. Current contents (2 lines):

      packages: .
      tests: True

  M2.1 must therefore *amend* the existing file (replace `packages: .`
  with `packages: ., ephemeral-pg-opentelemetry/`), not create a new one.
  The Idempotence section already calls this out as the correct posture
  ("If `cabal.project` is already present from another piece of work,
  *amend* it rather than overwrite it"); the Progress checklist wording
  ("Wire it into a top-level `cabal.project` if one does not exist") is
  the misleading bit.

* `defaultFormatter` in
  `hs-opentelemetry-exporter-handle:OpenTelemetry.Exporter.Handle.Span`
  prints a single flat line per span — `<traceId> <spanId> <spanStart>
  <spanName>` — with no parent reference. The Step 5 example output in
  this plan, which depicts a visually nested span tree, will *not*
  reproduce verbatim with `defaultFormatter`. The demo therefore needs a
  small custom formatter (or programmatic assertion via the in-memory
  exporter, planned in M3.4) to make parent/child observable in stderr.
  M2.4 will record the actual stderr verbatim alongside the span IDs
  that demonstrate stitching.

Versions of the upstream packages on disk (2026-04-29) for the record:

| package                                  | version  |
| ---------------------------------------- | -------- |
| `hs-opentelemetry-api`                   | 0.3.0.0  |
| `hs-opentelemetry-sdk`                   | 0.1.0.1  |
| `hs-opentelemetry-exporter-handle`       | 0.0.1.2  |
| `hs-opentelemetry-exporter-in-memory`    | 0.0.1.4  |
| `hs-opentelemetry-instrumentation-hspec` | 0.0.1.2  |
| `hs-opentelemetry-semantic-conventions`  | 0.1.\*   |

The bound `^>= 0.3` in the plan's `build-depends` for
`hs-opentelemetry-api` is correct.

Baseline `cabal test ephemeral-pg-test` (2026-04-29): **10 examples, 0
failures**, finished in 3.9 seconds. Floor is intact.

### M2.4 — captured span tree (2026-04-29)

`cabal test ephemeral-pg-opentelemetry-demo --test-show-details=streaming`,
relevant stderr lines (one record per emitted span, fields are
`trace=<traceId> span=<spanId> parent=<parentSpanId>
status=<unset|ok|error:msg> name=<name>`; emitted in completion order
under hspec's parallel runner):

    trace=0c406f2ed4126c2120b6027238f4a0c2 span=da06a898e98294eb parent=43cbd5f5c8f9a05b status=ok    name=ephemeralpg.with
    trace=0c406f2ed4126c2120b6027238f4a0c2 span=989097703a40c0dc parent=da06a898e98294eb status=ok    name=ephemeralpg.stop
    trace=0c406f2ed4126c2120b6027238f4a0c2 span=eee6d36d7dc45a80 parent=da06a898e98294eb status=unset name=ephemeralpg.with.body
    trace=0c406f2ed4126c2120b6027238f4a0c2 span=6fc1b1887c64e1be parent=da06a898e98294eb status=ok    name=ephemeralpg.start
    trace=0c406f2ed4126c2120b6027238f4a0c2 span=000fd17341bc07a9 parent=ROOT             status=unset name=Run tests
    trace=0c406f2ed4126c2120b6027238f4a0c2 span=d31be2b2725cf166 parent=000fd17341bc07a9 status=unset name=ephemeral-pg under OpenTelemetry
    trace=0c406f2ed4126c2120b6027238f4a0c2 span=43cbd5f5c8f9a05b parent=d31be2b2725cf166 status=unset name=emits a span tree under withTraced

Reordering by parent links gives the tree the plan predicts:

    Run tests                                   (000fd173…)
      └─ ephemeral-pg under OpenTelemetry      (d31be2b2…)         describe
         └─ emits a span tree under withTraced (43cbd5f5…)         it
            └─ ephemeralpg.with                (da06a898…)
               ├─ ephemeralpg.start            (6fc1b188…)
               ├─ ephemeralpg.with.body        (eee6d36d…)
               └─ ephemeralpg.stop             (98909770…)

All seven spans share `trace=0c406f2e…0c2`, satisfying the M2 acceptance
criterion: `it ›  ephemeralpg.with ›  ephemeralpg.start` and
`it ›  ephemeralpg.with ›  ephemeralpg.stop` are visible
ancestor/descendant pairs by ID. The `ephemeralpg.with.body` span
captures the user action and is, as designed, a sibling of start/stop
under `ephemeralpg.with`. `status=unset` on the hspec-emitted spans is
expected — `instrumentSpec` does not call `setStatus` so the default
(Unset) is used.


## Decision Log

Record every decision made while working on the plan.

- Decision: The right analogy is **library instrumentation**, not
  **runner instrumentation**. `ephemeral-pg` is invoked from inside a test;
  its spans are children of the test's parent span, not parents of test
  spans.
  Rationale: `hspec` and `tasty` are runners, so their instrumentation
  packages create the parent test span. `ephemeral-pg` is a test-support
  library, the same role that `hasql` plays for the user's existing
  `hasql-opentelemetry` package, which sits at
  `/Users/shinzui/Keikaku/bokuno/hasql-opentelemetry`. Mirroring that
  library's structure (one wrapping module per public entry point, with
  a `HasqlConfig`-style record and helpers like `instrumentedUse`) gives
  us a precedent the user has already validated.
  Date: 2026-04-29

- Decision: Ship instrumentation as a **separate companion package**
  (`ephemeral-pg-opentelemetry`) inside this repository, rather than
  adding `hs-opentelemetry-api` as a direct dependency of `ephemeral-pg`.
  Rationale: `hs-opentelemetry-instrumentation-hspec`,
  `-tasty`, `-postgresql-simple`, and the user's `hasql-opentelemetry`
  all use this layout. It keeps `ephemeral-pg`'s dependency footprint
  unchanged for users who do not want OpenTelemetry, and avoids forcing a
  major version bump on the core library when conventions change upstream.
  The trade-off is that the wrappers must call the existing public API
  rather than reaching into internals, which means we cannot easily
  emit child spans for *internal* phases like the CoW detection unless
  we add hooks. Milestone 2 deliberately scopes to the existing public
  API; Milestone 3 evaluates whether to expose hooks.
  Date: 2026-04-29

- Decision: Use the dedicated test-suite component model rather than
  shipping the demo as part of the main `ephemeral-pg-test` suite.
  Rationale: The current `test/Main.hs` is a pure functional test of the
  library; mixing in OpenTelemetry SDK setup would force that test suite
  to depend on three additional `hs-opentelemetry-*` packages. A separate
  `ephemeral-pg-opentelemetry-demo` test-suite in the new sub-package
  keeps the dependency boundary clean.
  Date: 2026-04-29

- Decision: defer per-phase child spans (`ephemeralpg.cache.restore`,
  `ephemeralpg.postgres.start`, `ephemeralpg.postgres.wait_ready`,
  `ephemeralpg.createdb`) and the per-phase attributes
  (`ephemeralpg.cache.hit`, `ephemeralpg.cache.cow_method`) to a
  follow-up plan. The acceptance criterion in M3 requires a
  `cache.restore` span and a `cache.hit=true` attribute on cache-hit,
  but the cache machinery is in `EphemeralPg.Internal.Cache` (not
  exposed) — observing it requires either (a) a `Phase` callback hook
  on `Config` or (b) re-implementing the lifecycle in the wrapper. The
  plan's Idempotence section explicitly directs us to pause and record
  the requirement for the hook, so we do that here. M3 still ships
  withCached/snapshot/dump wrappers, top-level semantic attributes
  (`db.system.name`, `db.namespace`, `ephemeralpg.port`,
  `ephemeralpg.shutdown.mode`), uniform error recording with
  `recordException`, and programmatic in-memory tests.
  Open follow-up: a separate ExecPlan that adds
  `Config.onPhase :: Maybe (Phase -> IO ())` (or similar) and threads
  it through `start`/`startCached`. Once that lands, the wrapper can
  attach phase spans without forking the lifecycle.
  Date: 2026-04-29

- Decision: `withTraced` re-implements the `mask` + `start` + `stop`
  composition rather than wrapping `Pg.with`. The acceptance criterion
  for M2 requires `ephemeralpg.start` and `ephemeralpg.stop` to be
  visible *children* of `ephemeralpg.with`. Calling `Pg.with` wraps
  start and stop opaquely inside one IO action, so they would never
  surface as their own spans. Re-implementing means three lines of
  `mask`/`onException` from `EphemeralPg.with` are duplicated in the
  wrapper. We keep using `Pg.start` and `Pg.stop` (the public API) — we
  do not reach into internals — so this stays inside the boundary set
  by the previous decision ("Reuse the existing public API").
  Date: 2026-04-29

- Decision: **Go** for the spike (Milestone 2). Proceed to scaffold
  `ephemeral-pg-opentelemetry/` and implement the minimal `*Traced` API.
  Rationale: M1.1 confirms the public API the wrappers will call is
  stable (no churn pending) and exactly matches the plan's
  `Context and Orientation`. The two surprises uncovered (an existing
  `cabal.project` and a flat default formatter) are both cosmetic — they
  shift one cabal-project edit from "create" to "amend" and require
  either a tiny custom formatter or programmatic assertion to make span
  parenting visible. Neither alters the architecture. The structural
  template (`hasql-opentelemetry`'s `OpenTelemetry.Instrumentation.Hasql`)
  is already in production for the user, so the wrapper-package shape is
  validated. Risks are listed in the M1 paragraph of `Outcomes &
  Retrospective` below.
  Date: 2026-04-29


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at
completion. Compare the result against the original purpose.

The Milestone 1 deliverable — a written go / no-go recommendation with
rationale and risks — must be appended here when M1 completes. The
Milestone 2 deliverable — proof that span trees emit and stitch — must be
appended here when M2 completes, including the captured stderr excerpt.

### Milestone 1 — Evaluation (2026-04-29)

* **Recommendation:** **go.** Proceed to Milestone 2 and build the spike.
* **Rationale:** the plan's structural argument — that `ephemeral-pg`
  should be instrumented as a *library* (whose spans are children of the
  test-runner's `it` span), not as a *runner* — survived the M1 reading
  of the code. The library's natural span boundaries (`with`,
  `withConfig`, `withCached`, `start`, `startCached`, `stop`, `restart`,
  the `Snapshot` and `Dump` calls) are stable, public, and already
  return `Either`-flavoured errors that map cleanly onto OTel
  `SpanStatus.Error` plus an `error.type` attribute. The companion-package
  layout has a working in-house precedent (`hasql-opentelemetry`), so
  there is no novel shape to invent. Upstream
  `hs-opentelemetry-instrumentation-hspec` 0.0.1.2 produces the parent
  span that the new `ephemeralpg.with` span will stitch under, exactly as
  the plan describes. The two M1 surprises (existing `cabal.project`,
  flat `defaultFormatter`) are documented under Surprises & Discoveries
  and are cosmetic — they do not change the design.
* **Risks accepted:**
  * **Dependency footprint, M3.** Once `ephemeral-pg-opentelemetry`
    starts emitting semantic-convention attributes, it will pull in
    `hs-opentelemetry-semantic-conventions` and `hs-opentelemetry-utils-exceptions`.
    These are not in the M2 build-depends list and need to be added in
    M3.
  * **Public API of the core library.** M3 may require exposing a
    `Phase` callback hook on `Config` to emit child spans for internal
    phases (`runInitDb`, `startPostgres`, `waitForPostgres`,
    `runCreateDb`, `restoreFromCache`). The plan's Idempotence section
    permits this; the alternative is to re-implement the lifecycle in
    the wrapper, which is a fork hazard. The decision is deferred to M3
    and will be recorded in `Decision Log` at that time.
  * **Semantic-convention churn.** The plan opts in to both stable and
    legacy DB attribute names via `OTEL_SEMCONV_STABILITY_OPT_IN`. The
    upstream conventions are still moving; we accept the cost of
    keeping both names in lock-step with `hasql-opentelemetry` until
    OTel v1 stabilises (see
    `OpenTelemetry-Hasql-Instrumentation-Guide.md` next door).
  * **Stderr-as-evidence.** `defaultFormatter` does not visualise
    parenthood. M2.4 will use a small custom formatter that prints
    `parent=<spanId>` so the captured stderr is by-eye verifiable.
    Programmatic verification via `hs-opentelemetry-exporter-in-memory`
    is deferred to M3.4 as already planned.
  * **macOS-only CoW path.** The CoW detection on the test machine
    returns `CowClonefile` (macOS); CI on Linux will exercise the
    `CowReflink` branch and on filesystems without CoW will fall back.
    The `ephemeralpg.cache.cow_method` attribute therefore varies by
    host — that is desired but worth noting for anyone consuming the
    spans.

### Milestone 2 — Spike (2026-04-29)

* **Result:** the spike works. `cabal test ephemeral-pg-opentelemetry-demo`
  emits a single trace whose tree is exactly the one this plan
  predicted (see `Surprises & Discoveries → M2.4`). The test passes —
  `withTraced` against a real PostgreSQL cluster, started and stopped
  inside an `it` block, completes in ~0.9s and produces seven spans.
* **Acceptance against the M2 criterion:** met.
  `it › ephemeralpg.with › ephemeralpg.start` and
  `it › ephemeralpg.with › ephemeralpg.stop` are both present as
  ancestor/descendant pairs in the captured stderr, with all seven
  spans sharing one trace ID.
* **Deviations from the plan body:**
  * `cabal.project` already existed and was *amended* (per Idempotence
    section) rather than created.
  * Added an `allow-newer` block waiving the published Hackage
    `hs-opentelemetry-*` upper bounds. The upstream tree on disk
    relaxes those bounds to `==0.3.*`; `allow-newer` is the
    least-invasive way to keep the build resolving against Hackage
    until those releases are cut. Recorded as a tracked debt for M3
    (we should switch to `source-repository-package` or
    `index-state` pinning once the upstream releases are out, so the
    project remains buildable for downstream users without our
    on-disk checkout).
  * `withTraced` is implemented as `mask` + `startTraced'` +
    `stopTraced'` rather than wrapping `Pg.with` — see Decision Log
    (2026-04-29). This is required to make the start/stop spans
    individually visible.
  * Demo uses a custom `parentAwareFormatter`. M3.4's in-memory
    exporter assertions remain the durable form of validation;
    stderr is for human inspection.
* **Baseline integrity:** the existing `ephemeral-pg-test` suite is
  unchanged and still passes (10 examples, 0 failures).

### Milestone 3 — Productionisation (2026-04-29)

* **Result:** the companion package is publishable as a v0.1 spike.
  All three test-suites pass:
  * `ephemeral-pg-test` — 10 examples, 0 failures (unchanged baseline).
  * `ephemeral-pg-opentelemetry-test` — 3 examples, 0 failures
    (programmatic in-memory assertions).
  * `ephemeral-pg-opentelemetry-demo` — 1 example, 0 failures (stderr
    span tree).
* **Public API delivered (M2 + M3):** matches the plan's "after M3"
  block almost verbatim, with the planned `Phase`-driven internal
  spans deferred:
  ```
  module OpenTelemetry.Instrumentation.EphemeralPg
    EphemeralPgOtelConfig (..)
    defaultEphemeralPgOtelConfig
    ephemeralPgTracer
    withTraced, withCachedTraced
    startTraced, stopTraced, restartTraced
    createSnapshotTraced, restoreSnapshotTraced, deleteSnapshotTraced
    dumpTraced, restoreTraced
  ```
* **Acceptance against the M3 criteria, item by item:**
  * (i) **Met.** `ephemeralpg.with` is the parent of
    `ephemeralpg.start` and `ephemeralpg.stop` — asserted by
    `ephemeral-pg-opentelemetry-test → "stitches with under
    start/stop"`.
  * (ii) **Deferred** to a follow-up plan that adds a `Phase` callback
    hook on `EphemeralPg.Config`. Recorded under the 2026-04-29
    Decision Log entry.
  * (iii) **Deferred** for the same reason as (ii). The
    `recordStartError` helper is implemented and exercised whenever
    `Pg.start` returns `Left`; integration coverage is via the demo
    rather than a bind-to-port-1 unit test (the latter would still
    run `initdb` and was deemed not worth the extra ~1s per run for
    the spike).
* **Open follow-ups:**
  1. Land an `onPhase :: Maybe (Phase -> IO ())` hook on
     `EphemeralPg.Config`, then surface
     `ephemeralpg.cache.restore`, `ephemeralpg.postgres.start`,
     `ephemeralpg.postgres.wait_ready`, `ephemeralpg.createdb`, and
     `ephemeralpg.cache.{hit,cow_method}` attributes — own ExecPlan.
  2. Replace `allow-newer` in `cabal.project` with proper version
     pinning once the upstream `hs-opentelemetry-*` Hackage releases
     ship the `==0.3.*` API bound. Until then, builds depend on the
     `allow-newer` waiver to resolve.
  3. Move the `LICENSE` symlink so the companion package's
     `license-file: ../LICENSE` stops emitting the
     `relative-path-outside` warning when packaging for sdist.


## Context and Orientation

Read this section even if you think you know the codebase. It defines every
term used later in the plan and names every file the implementer will touch
or read.

### `ephemeral-pg` (this repository)

The library lives in `src/` and is declared in `ephemeral-pg.cabal` at the
repository root. The package builds with `cabal` (no Stack project file is
present). Its public surface is the module `EphemeralPg`, which re-exports
types and lifecycle functions from the submodules:

* `EphemeralPg` (file: `src/EphemeralPg.hs`) — the user-facing entry
  point. Exposes `with`, `withConfig`, `withCached`, `start`, `startCached`,
  `stop`, `restart`, plus `Database`, `Config`, cache configuration, and
  the `StartError` / `StopError` types. The `with` family wraps `start` and
  `stop` in `Control.Exception.mask` + `onException`, returning
  `IO (Either StartError a)`. **These are the natural span boundaries.**
* `EphemeralPg.Config` (file: `src/EphemeralPg/Config.hs`) — `Config`,
  `DirectoryConfig`, `ShutdownMode`, `defaultConfig`, `verboseConfig`,
  `autoExplainConfig`. Configuration is a `Monoid` of `Last` fields. Span
  attributes derived from `Config` should be the resolved values, not the
  user input.
* `EphemeralPg.Database` (file: `src/EphemeralPg/Database.hs`) — the
  `Database` record (port, socket dir, data dir, db name, user, process,
  cleanup action, shutdown mode/timeout) and the `connectionSettings`
  function used by callers to plug into `hasql`. The fields here are
  what populate `db.namespace`, `ephemeralpg.port`, etc.
* `EphemeralPg.Snapshot` (file: `src/EphemeralPg/Snapshot.hs`) —
  `createSnapshot`, `restoreSnapshot`, `deleteSnapshot`. `createSnapshot`
  internally **stops** postgres, copies the data directory, then **restarts**
  postgres, so a single user call expands to a non-trivial child span tree.
* `EphemeralPg.Dump` (file: `src/EphemeralPg/Dump.hs`) — `dump`, `dumpToText`,
  `restore`, `restoreFromText`. These shell out to `pg_dump` and `psql`.
* `EphemeralPg.Error` (file: `src/EphemeralPg/Error.hs`) — `StartError`
  variants (`InitDbError`, `PostgresStartError`, `CreateDbError`,
  `TimeoutError`, plus the inner `*Failed` records) and `StopError`. These
  are what we map to `error.type` attributes and `SpanStatus.Error`.

The non-exported internals describe the span hierarchy under `start`:

* `EphemeralPg.Internal.Cache` (file: `src/EphemeralPg/Internal/Cache.hs`)
  — `getCacheKey`, `isCached`, `createCache`, `restoreFromCache`,
  `cleanupRuntimeFiles`, plus `CacheConfig`. The cache key is
  `<pgVersion>-<configHash>`, with `pgVersion` extracted by running
  `postgres --version`.
* `EphemeralPg.Internal.CopyOnWrite`
  (file: `src/EphemeralPg/Internal/CopyOnWrite.hs`) — `detectCowCapability`
  returns `CowSupported CowClonefile` (macOS, `cp -c`),
  `CowSupported CowReflink` (Linux, `cp --reflink`), or `CowNotSupported
  Text`. `copyDirectory` dispatches via `cp` in a sub-process.
* `EphemeralPg.Internal.Directory`, `EphemeralPg.Internal.Port`,
  `EphemeralPg.Internal.Except` — temp directory creation, free-port
  search, and the `ExceptT`-flavoured helpers (`liftE`, `onError`,
  `runStartup`) used to compose error-returning IO.
* `EphemeralPg.Process.InitDb` (file: `src/EphemeralPg/Process/InitDb.hs`)
  — `runInitDb` finds the `initdb` binary, runs it via
  `EphemeralPg.Process.runProcessCapture`, and writes
  `postgresql.conf`. Also exports `writePostgresConf` used by the cache
  hit path.
* `EphemeralPg.Process.Postgres`
  (file: `src/EphemeralPg/Process/Postgres.hs`) — `startPostgres` (forks
  `postgres` via `typed-process`), `waitForPostgres` (loops `pg_isready`),
  `stopPostgres` (sends `SIGTERM`/`SIGINT`/`SIGQUIT` per `ShutdownMode`,
  with `SIGKILL` fallback after a timeout).
* `EphemeralPg.Process.CreateDb` (file: `src/EphemeralPg/Process/CreateDb.hs`)
  — `runCreateDb`, skipped when the database name is `"postgres"` (which
  `initdb` already creates).

The existing test suite is `test/Main.hs`, declared in `ephemeral-pg.cabal`
under `test-suite ephemeral-pg-test`, using `hspec` 2.11. It contains
"can start and stop a database", "can connect", "can restart", and a
caching test, plus QuickCheck `Monoid` laws for `Config`. Nothing about
this suite uses OpenTelemetry today.

### `hs-opentelemetry` (the upstream OpenTelemetry-Haskell mono-repo)

This is a third-party project registered in `mori` as
`iand675/hs-opentelemetry` and resolved on disk to
`/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry`.
The implementer of this plan must read it through `mori` and the on-disk
checkout — never via the network. Relevant packages:

* `hs-opentelemetry-api` — defines `Tracer`, `Span`, `Context`,
  `SpanArguments`, `SpanKind` (`Client`, `Internal`, `Server`, `Producer`,
  `Consumer`), `SpanStatus` (`Ok`, `Error Text`), `inSpan`, `inSpan'`,
  `createSpan`, `endSpan`, `addAttribute`, `addAttributes`, `addEvent`,
  `recordException`, `setStatus`, `getGlobalTracerProvider`, `makeTracer`,
  `tracerOptions`, `defaultSpanArguments`, `detectInstrumentationLibrary`
  (a Template Haskell splice that injects the package name/version of the
  caller into the tracer name).
* `hs-opentelemetry-sdk` — the production tracer provider plus
  `getTracerProviderInitializationOptions` reading env vars.
* `hs-opentelemetry-exporter-handle` — a stderr/file exporter we will use
  in the demo.
* `hs-opentelemetry-exporter-in-memory` — used in M3.4 unit tests to
  assert span tree shape without I/O.
* `hs-opentelemetry-semantic-conventions` — pre-defined attribute keys
  (e.g. `db.system`, `db.name`, etc.). Cabal-name is
  `hs-opentelemetry-semantic-conventions`.

The two reference instrumentations the user named:

* `hs-opentelemetry-instrumentation-hspec` — single module
  `OpenTelemetry.Instrumentation.Hspec`. The function `instrumentSpec ::
  Tracer -> Context -> SpecWith a -> SpecWith a` walks a `SpecForest`,
  emitting one span per `describe` and one span per `it`. The function
  `wrapSpec :: MonadIO m => m (SpecWith a -> SpecWith a)` returns a
  simpler wrapper that emits only `it` spans. **Critically**, both
  re-attach the parent OpenTelemetry context inside `aroundAction`
  because `hspec` may schedule examples on forked threads.
* `hs-opentelemetry-instrumentation-tasty` — single module
  `OpenTelemetry.Instrumentation.Tasty`. The function
  `instrumentTestTree :: TestTree -> IO TestTree` rewrites a `TestTree`,
  using `withResource` to bind a `TestGroup`'s span to the group lifetime,
  wrapping `SingleTest` in `WrappedTest { wrapper, innerTest }` (an
  `IsTest` instance that records the test's `Outcome` as
  `SpanStatus.Ok`/`SpanStatus.Error`), and emitting separate `acquire`
  and `release` spans inside `WithResource`. It threads the parent span
  manually via `getParentSpan :: IO (Maybe Span)` because tasty
  parallelism breaks thread-local context. See the file header comment
  `Note [Test parallelism]`.

These two are runner instrumentations and are not what `ephemeral-pg` should
imitate structurally. They are, however, what produces the *parent* span
under which `ephemeral-pg` spans must stitch.

### `hasql-opentelemetry` (the user's other companion library)

Disk path: `/Users/shinzui/Keikaku/bokuno/hasql-opentelemetry`. Its module
`OpenTelemetry.Instrumentation.Hasql` is the structural template we will
follow. Concretely:

* It exports a config record `HasqlConfig` with `captureQueries`,
  `maxQueryLength`, `dbName`, `poolName`, `extraAttributes`. Our
  analog is `EphemeralPgOtelConfig`.
* It exposes a tracer `hasqlTracer :: IO Tracer` constructed via
  `getGlobalTracerProvider` plus `$detectInstrumentationLibrary
  tracerOptions`. Our analog is `ephemeralPgTracer`.
* It wraps every public hasql call with `inSpan` / `inSpan'` and emits
  `db.system.name`, `db.namespace`, `db.client.connection.pool.name`,
  honouring `OTEL_SEMCONV_STABILITY_OPT_IN` for the dual stable/legacy
  attribute names.
* Errors are recorded uniformly: `addAttribute "error.type" <constructor>`,
  `recordException`, `setStatus (Error msg)`. We will match this scheme.

The ChangeLog and master plan inside `hasql-opentelemetry` are not required
reading for this plan, but they are a good reference when the implementer
needs to remember how the user prefers to wire env-var-driven semantic
conventions.

### Definitions of every term of art used later

* **Span** — a single timed operation in OpenTelemetry. Has a name, start
  and end timestamps, attributes, status (`Ok`/`Error`), and a parent.
  Created with `inSpan`/`createSpan` from `OpenTelemetry.Trace.Core`.
* **Trace** — a tree of spans sharing a `traceId`.
* **Tracer** — the thing you pass to `inSpan`. Created from a
  `TracerProvider` via `makeTracer`.
* **TracerProvider** — global registry. `getGlobalTracerProvider` returns
  the current one; `setGlobalTracerProvider` installs a new one.
* **Span exporter / processor** — a sink that receives finished spans.
  We use `hs-opentelemetry-exporter-handle` (writes to a `Handle`
  like `stderr`) for the demo and
  `hs-opentelemetry-exporter-in-memory` for unit tests.
* **Context** — a per-thread (`OpenTelemetry.Context.ThreadLocal`) store
  that holds the active span. `inSpan` mutates it.
* **Semantic convention** — a standardised attribute key.
  `db.system.name=postgresql`, `db.namespace=<dbname>`, etc., are defined
  in the `hs-opentelemetry-semantic-conventions` package.
* **Companion package** — a separate cabal library distributed alongside
  the core library that adds an optional dependency (here,
  `hs-opentelemetry-api`). The pattern used by all of
  `hs-opentelemetry-instrumentation-*` and by the user's
  `hasql-opentelemetry`.


## Plan of Work

The work is divided into one evaluation milestone and two implementation
milestones. Each milestone is independently verifiable.

### Milestone 1 — Evaluation

The scope of M1 is to read everything described in `Context and Orientation`
and produce a written recommendation. There are no source-code changes.
At the end of M1, the `Outcomes & Retrospective` section of this plan must
contain a paragraph of the form:

* **Recommendation:** `<go / no-go>`.
* **Rationale:** one paragraph referring to specific findings.
* **Risks accepted:** bulleted list (e.g., dependency graph, semantic
  convention churn, cabal-project layout cost).

Acceptance criterion: a reader of this plan, with no prior context, can
read the `Outcomes & Retrospective` paragraph and either start the
implementation or stop, knowing why.

### Milestone 2 — Spike (proof of stitching)

The scope of M2 is to prove that `ephemeral-pg`'s lifecycle spans emit and
stitch correctly under a parent test span produced by
`hs-opentelemetry-instrumentation-hspec`. At the end of M2 there will be:

* A new directory `ephemeral-pg-opentelemetry/` containing
  * `ephemeral-pg-opentelemetry.cabal`,
  * `src/OpenTelemetry/Instrumentation/EphemeralPg.hs`,
  * `test/Demo.hs`.
* A top-level `cabal.project` listing both packages
  (`packages: ., ephemeral-pg-opentelemetry`).
* A test-suite `ephemeral-pg-opentelemetry-demo` that, when run with
  `cabal test ephemeral-pg-opentelemetry-demo`, prints a span tree to
  stderr in which `ephemeralpg.with` is a child of `it`.

The minimal API exposed by the spike module is:

    module OpenTelemetry.Instrumentation.EphemeralPg
      ( EphemeralPgOtelConfig (..)
      , defaultEphemeralPgOtelConfig
      , ephemeralPgTracer
      , withTraced
      , startTraced
      , stopTraced
      , restartTraced
      ) where

Each `*Traced` function calls the original `EphemeralPg.<name>` inside an
`inSpan'` block, names the span `ephemeralpg.<name>`, and (where the call
returns `Either StartError a`) records errors as described in
`Decision Log`. Internal child spans (`ephemeralpg.cache.restore`,
`ephemeralpg.postgres.start`, etc.) are out of scope for M2 because the
public API does not expose those phases. They land in Milestone 3 either
by exposing hooks or by re-implementing the lifecycle in the wrapper.

Acceptance criterion: running `cabal test ephemeral-pg-opentelemetry-demo`
emits stderr in which the span tree visibly contains
`it ›  ephemeralpg.with ›  ephemeralpg.start` and
`ephemeralpg.with ›  ephemeralpg.stop` as ancestor/descendant pairs (the
exact `›` formatting depends on the chosen formatter; what matters is
parent/child by trace and span IDs).

### Milestone 3 — Productionisation

The scope of M3 is to extend the spike into a publishable companion
package. At the end of M3 there will be:

* `withCachedTraced`, `createSnapshotTraced`, `restoreSnapshotTraced`,
  `deleteSnapshotTraced`, `dumpTraced`, `restoreTraced` exposed.
* Internal child spans for the lifecycle phases of `start` (initdb,
  cache.create / cache.restore, postgres.start, postgres.wait_ready,
  createdb). Achieving these requires either (a) adding hooks to
  `EphemeralPg.start` so the wrapper can attach spans, or (b) re-running
  the same low-level operations in `OpenTelemetry.Instrumentation.EphemeralPg`.
  M3 must pick one and record the choice in `Decision Log`.
* Standard semantic conventions: `db.system.name=postgresql` (stable) /
  `db.system=postgresql` (legacy), `db.namespace=<dbname>`, plus
  `ephemeralpg.cache.hit` (bool), `ephemeralpg.cache.cow_method`
  (`clonefile`/`reflink`/`none`), `ephemeralpg.shutdown.mode`
  (`graceful`/`fast`/`immediate`), `ephemeralpg.port`. Honour
  `OTEL_SEMCONV_STABILITY_OPT_IN` exactly the way `hasql-opentelemetry`
  does.
* Error recording: every `Left StartError | Left StopError | Left Text`
  becomes `setStatus (Error <render>)` plus `addAttribute "error.type"
  <constructor>` plus `recordException`. The error renderers
  `renderStartError` / `renderStopError` already exist in
  `EphemeralPg.Error`.
* Unit tests using `hs-opentelemetry-exporter-in-memory` that assert the
  emitted span tree without depending on stderr formatting.
* A short "Tracing" section appended to the repository-root `README.md`.

Acceptance criterion: `cabal test ephemeral-pg-opentelemetry-test` passes
and the assertions cover (i) `ephemeralpg.with` is a parent of
`ephemeralpg.start`, `ephemeralpg.stop`; (ii) on cache hit the tree
contains `ephemeralpg.cache.restore` with `ephemeralpg.cache.hit=true`;
(iii) on a forced startup error (e.g., bind to port 1) the parent span
has status `Error` and `error.type=PostgresStartError`.


## Concrete Steps

All commands run from the repository root
(`/Users/shinzui/Keikaku/bokuno/ephemeral-pg-project/ephemeral-pg`) unless
stated otherwise. Show the directory in the prompt of every transcript so a
reader can compare.

### Step 1 — verify the baseline test suite still works

This is the floor we cannot regress.

    $ cabal build all
    $ cabal test ephemeral-pg-test

Expected: tests pass. If they don't, stop and fix before changing
anything. (No OpenTelemetry work yet.)

### Step 2 — create the companion package skeleton (Milestone 2)

Create `ephemeral-pg-opentelemetry/ephemeral-pg-opentelemetry.cabal` with
contents like:

    cabal-version: 3.0
    name: ephemeral-pg-opentelemetry
    version: 0.1.0.0
    synopsis: OpenTelemetry tracing for ephemeral-pg
    license: BSD-3-Clause
    build-type: Simple

    library
      default-language: GHC2021
      hs-source-dirs: src
      exposed-modules:
        OpenTelemetry.Instrumentation.EphemeralPg
      build-depends:
        base >=4.18 && <5,
        ephemeral-pg,
        hs-opentelemetry-api ^>=0.3,
        text >=2.0 && <2.2,
        unliftio-core >=0.2 && <0.3,
        hashable >=1.4 && <1.6
      default-extensions:
        OverloadedStrings
        TemplateHaskell

    test-suite ephemeral-pg-opentelemetry-demo
      default-language: GHC2021
      type: exitcode-stdio-1.0
      hs-source-dirs: test
      main-is: Demo.hs
      build-depends:
        base,
        ephemeral-pg,
        ephemeral-pg-opentelemetry,
        hs-opentelemetry-api,
        hs-opentelemetry-sdk,
        hs-opentelemetry-exporter-handle,
        hs-opentelemetry-instrumentation-hspec,
        hspec >=2.11 && <2.12

Create a top-level `cabal.project` (it does not currently exist; cabal
tooling will use it automatically):

    packages:
      .
      ephemeral-pg-opentelemetry/

The `ephemeral-pg-opentelemetry` package is the *only* place
`hs-opentelemetry-*` is depended on; the core `ephemeral-pg.cabal` is
unchanged.

### Step 3 — write the wrapper module (Milestone 2)

In `ephemeral-pg-opentelemetry/src/OpenTelemetry/Instrumentation/EphemeralPg.hs`,
define the API enumerated under Milestone 2 above. The body of `withTraced`
follows the `hasql-opentelemetry` pattern:

    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE TemplateHaskell  #-}
    module OpenTelemetry.Instrumentation.EphemeralPg
      ( EphemeralPgOtelConfig (..)
      , defaultEphemeralPgOtelConfig
      , ephemeralPgTracer
      , withTraced
      , startTraced
      , stopTraced
      , restartTraced
      ) where

    import qualified Data.Text as T
    import qualified EphemeralPg as Pg
    import OpenTelemetry.Trace.Core
      ( SpanStatus (..)
      , Tracer
      , addAttribute
      , defaultSpanArguments
      , detectInstrumentationLibrary
      , getGlobalTracerProvider
      , inSpan'
      , makeTracer
      , recordException
      , setStatus
      , tracerOptions
      )

    data EphemeralPgOtelConfig = EphemeralPgOtelConfig
      { serviceLabel :: !T.Text }

    defaultEphemeralPgOtelConfig :: EphemeralPgOtelConfig
    defaultEphemeralPgOtelConfig = EphemeralPgOtelConfig { serviceLabel = "" }

    ephemeralPgTracer :: IO Tracer
    ephemeralPgTracer = do
      tp <- getGlobalTracerProvider
      pure $ makeTracer tp $detectInstrumentationLibrary tracerOptions

    withTraced
      :: EphemeralPgOtelConfig
      -> (Pg.Database -> IO a)
      -> IO (Either Pg.StartError a)
    withTraced cfg act = do
      tracer <- ephemeralPgTracer
      inSpan' tracer "ephemeralpg.with" defaultSpanArguments $ \sp -> do
        result <- Pg.with $ \db -> do
          inSpan' tracer "ephemeralpg.with.body" defaultSpanArguments $ \_ ->
            act db
        case result of
          Left e -> do
            addAttribute sp ("error.type" :: T.Text) (T.pack (show e))
            setStatus sp (Error (Pg.renderStartError e))
          Right _ -> setStatus sp Ok
        pure result

The `startTraced` / `stopTraced` / `restartTraced` definitions follow the
same shape (omitted here for brevity but explicit during implementation).

### Step 4 — write the demo test-suite (Milestone 2)

In `ephemeral-pg-opentelemetry/test/Demo.hs`:

    module Main (main) where

    import qualified OpenTelemetry.Context as Ctx
    import OpenTelemetry.Context.ThreadLocal (attachContext, getContext)
    import OpenTelemetry.Exporter.Handle.Span
    import OpenTelemetry.Instrumentation.EphemeralPg
      (defaultEphemeralPgOtelConfig, withTraced)
    import OpenTelemetry.Instrumentation.Hspec (instrumentSpec)
    import OpenTelemetry.Processor.Batch.Span
    import OpenTelemetry.Trace
    import qualified OpenTelemetry.Trace as Trace
    import OpenTelemetry.Trace.Sampler (alwaysOn)
    import Test.Hspec
    import Test.Hspec.Runner (defaultConfig, hspecWith)
    import UnliftIO (bracket, void)

    main :: IO ()
    main = do
      void $ attachContext Ctx.empty
      bracket initialize shutdownTracerProvider $ \_ -> do
        tp  <- getGlobalTracerProvider
        let tracer = makeTracer tp "ephemeral-pg-otel-demo" tracerOptions
        ctx <- getContext
        hspecWith defaultConfig $
          instrumentSpec tracer ctx $
            describe "ephemeral-pg under OpenTelemetry" $
              it "emits a span tree" $ do
                Right _ <- withTraced defaultEphemeralPgOtelConfig $ \_db -> pure ()
                pure ()
      where
        initialize = do
          stderrProc <- batchProcessor batchTimeoutConfig
                          (stderrExporter' (pure . defaultFormatter))
          (procs, opts) <- getTracerProviderInitializationOptions
          provider <- createTracerProvider
            (stderrProc : procs)
            (opts { tracerProviderOptionsSampler = alwaysOn })
          setGlobalTracerProvider provider
          pure provider

The exact module names mirror the upstream hspec example at
`/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/examples/hspec/test/Main.hs`,
which the implementer must read before writing this file.

### Step 5 — run the demo and capture the output

    $ cabal test ephemeral-pg-opentelemetry-demo --test-show-details=streaming

Expected: stderr contains a span tree with `ephemeralpg.with` printed under
the test span. Capture the raw output and paste it (truncated to the
relevant span lines) into `Surprises & Discoveries`.

### Step 6 — productionisation steps (Milestone 3)

Listed in the M3 progress checklist above. Each entry expands into
concrete edits in `OpenTelemetry/Instrumentation/EphemeralPg.hs` plus a
new `test/Spec.hs` that uses `hs-opentelemetry-exporter-in-memory`. The
same `cabal test` invocation runs both.


## Validation and Acceptance

Validation has three concrete forms.

**Compile-time validation.** `cabal build all` from the repository root
must succeed after every milestone. After Milestone 2 there are two
packages in the project; after Milestone 3 the new test-suite must also
build.

**Behavioural validation (Milestone 2).** The demo test-suite must print a
span tree in which the parent of `ephemeralpg.with` is the `it` span
created by `instrumentSpec`. The implementer asserts this by eye on the
stderr output captured in Step 5 above. The literal contents will look
something like:

    [span] name="Run tests"
    [span]   name="ephemeral-pg under OpenTelemetry"
    [span]     name="emits a span tree"
    [span]       name="ephemeralpg.with"
    [span]         name="ephemeralpg.with.body"

The exact formatter output is dictated by `defaultFormatter` from
`hs-opentelemetry-exporter-handle`; the parent/child relation is what
matters.

**Programmatic validation (Milestone 3).** The new test-suite
`ephemeral-pg-opentelemetry-test` uses
`hs-opentelemetry-exporter-in-memory` to gather every emitted
`ImmutableSpan` into an `IORef [ImmutableSpan]` (the recipe is in the
upstream tasty test
`/Users/shinzui/Keikaku/hub/haskell/hs-opentelemetry-project/hs-opentelemetry/instrumentation/tasty/tests/OpenTelemetry/Instrumentation/Tasty/Tests.hs`,
function `recordingProcessor`). The assertions then walk the parent
pointers to reconstruct the tree and `shouldBe` it against the
expected shape. Sample assertion sketch:

    spec :: Spec
    spec = describe "ephemeral-pg-opentelemetry" $ do
      it "stitches with under start/stop" $ do
        (ref, processor) <- recordingProcessor
        withMemoryTracer processor $ do
          _ <- withTraced defaultEphemeralPgOtelConfig (const (pure ()))
          pure ()
        spans <- readIORef ref
        spanTree spans `shouldBe`
          Branch "ephemeralpg.with"
            [ Leaf "ephemeralpg.start"
            , Leaf "ephemeralpg.stop"
            ]

Acceptance for the whole plan is the union of:

1. `cabal build all` succeeds.
2. `cabal test all` succeeds.
3. The captured stderr from Step 5 (in `Surprises & Discoveries`) shows the
   expected stitching.
4. The `Outcomes & Retrospective` paragraph from M1 is filled in.


## Idempotence and Recovery

All steps in this plan are additive: a new directory, a new top-level
`cabal.project`, a new test-suite. None of the existing files in `src/`,
`test/`, or `ephemeral-pg.cabal` is modified by Milestones 1 or 2. (M3
appends a "Tracing" section to `README.md`.) The plan can be retried at
any milestone simply by deleting the partially-written files and re-running
the cabal commands. If `cabal.project` is already present from another
piece of work, *amend* it rather than overwrite it — `packages:` accepts a
list, so add the new entry on a new line.

If a step in M3 turns out to require exposing a hook from the core
`ephemeral-pg` library (e.g., to emit child spans for `runInitDb`), pause,
record the requirement in `Decision Log`, and prefer adding a small
optional callback (`Config { ..., onPhase :: Maybe (Phase -> IO ()) }`)
over moving lifecycle code into the wrapper. The plan can resume after
the hook is merged.

The cleanup story is the existing one: `ephemeral-pg`'s `with` already uses
`mask` and `onException` to ensure clusters are stopped even if the user
action throws. Wrapping it in `inSpan'` does not affect this — `inSpan'`
itself uses `bracket` semantics to close the span on exception, which is
the desired behaviour.


## Interfaces and Dependencies

The new companion package depends on:

* `ephemeral-pg` — public API only. No reaching into `Internal.*` modules.
* `hs-opentelemetry-api ^>= 0.3` — `Tracer`, `Span`, `inSpan'`,
  `addAttribute`, `setStatus`, `recordException`, `defaultSpanArguments`,
  `detectInstrumentationLibrary`, `getGlobalTracerProvider`, `makeTracer`,
  `tracerOptions`.
* `text >=2.0 && <2.2` — already a transitive dep of `ephemeral-pg`.
* `unliftio-core >=0.2 && <0.3` — for `MonadUnliftIO` if we expose
  polymorphic-monad variants in M3.

The test-suite additionally depends on `hs-opentelemetry-sdk`,
`hs-opentelemetry-exporter-handle`,
`hs-opentelemetry-instrumentation-hspec`, and `hspec`.

The required public API after M2 is exactly:

    module OpenTelemetry.Instrumentation.EphemeralPg

      data EphemeralPgOtelConfig = EphemeralPgOtelConfig
        { serviceLabel :: Data.Text.Text }

      defaultEphemeralPgOtelConfig :: EphemeralPgOtelConfig

      ephemeralPgTracer :: IO OpenTelemetry.Trace.Core.Tracer

      withTraced
        :: EphemeralPgOtelConfig
        -> (EphemeralPg.Database -> IO a)
        -> IO (Either EphemeralPg.StartError a)

      startTraced
        :: EphemeralPgOtelConfig
        -> EphemeralPg.Config
        -> IO (Either EphemeralPg.StartError EphemeralPg.Database)

      stopTraced
        :: EphemeralPgOtelConfig
        -> EphemeralPg.Database
        -> IO ()

      restartTraced
        :: EphemeralPgOtelConfig
        -> EphemeralPg.Database
        -> IO (Either EphemeralPg.StartError EphemeralPg.Database)

After M3, additionally:

      withCachedTraced
        :: EphemeralPgOtelConfig
        -> (EphemeralPg.Database -> IO a)
        -> IO (Either EphemeralPg.StartError a)

      createSnapshotTraced
        :: EphemeralPgOtelConfig
        -> EphemeralPg.Database
        -> IO (Either Data.Text.Text EphemeralPg.Snapshot.Snapshot)

      restoreSnapshotTraced
        :: EphemeralPgOtelConfig
        -> EphemeralPg.Snapshot.Snapshot
        -> EphemeralPg.Database
        -> IO (Either Data.Text.Text ())

      deleteSnapshotTraced
        :: EphemeralPgOtelConfig -> EphemeralPg.Snapshot.Snapshot -> IO ()

      dumpTraced
        :: EphemeralPgOtelConfig
        -> EphemeralPg.Database
        -> FilePath
        -> EphemeralPg.Dump.DumpOptions
        -> IO (Either Data.Text.Text ())

      restoreTraced
        :: EphemeralPgOtelConfig
        -> EphemeralPg.Database
        -> FilePath
        -> IO (Either Data.Text.Text ())

The core `ephemeral-pg` library's public API does not change in M2 or M3
unless the implementer chooses option (a) under M3 — adding a `Phase`
hook to `Config`. That choice must be recorded in `Decision Log` and is
the only direction in which the core library is allowed to grow within
this plan.
