---
id: 2
slug: wire-config-stdout-and-config-stderr-to-the-postgres-process
title: "Wire Config.stdout and Config.stderr to the postgres process"
kind: exec-plan
created_at: 2026-05-19T00:49:42Z
intention: "intention_01kryvb3xhehv8dhd6fwh1thk1"
---

# Wire Config.stdout and Config.stderr to the postgres process

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.


## Purpose / Big Picture

`ephemeral-pg`'s public `Config` record (in
`src/EphemeralPg/Config.hs`) advertises two fields — `stdout :: Last (Maybe
Handle)` and `stderr :: Last (Maybe Handle)` — that are documented as
"Handle for postgres stdout (Nothing = discard)" and "Handle for postgres
stderr (Nothing = discard)". The intent of these fields, judging by their
types and Haddocks, is that a caller can write

```haskell
h <- openFile "pg.log" WriteMode
let cfg = defaultConfig <> mempty { stderr = Last (Just (Just h)) }
withConfig cfg action
```

and the postgres server's stderr stream will be redirected to that handle.
Today, that does not work. `EphemeralPg.Process.Postgres.startPostgres`
(`src/EphemeralPg/Process/Postgres.hs:66-79`) builds its `ProcessConfig`
with `setStdout nullStream` and `setStderr nullStream` hardcoded, ignoring
`config.stdout` and `config.stderr` entirely. The handle the caller
supplied is silently discarded; postgres's output goes to `/dev/null`
regardless of what the caller asked for. The field is, in effect, an
unimplemented API.

After this plan, the implementation matches the documented intent.
Concretely, a caller who passes
`mempty { stderr = Last (Just (Just h)) }` will see postgres's stderr
arrive on `h` (any `LOG`, `ERROR`, `FATAL`, or `auto_explain` output that
postgres would normally write to stderr). A caller who passes nothing
keeps today's behaviour (stderr discarded). The library's existing tests
that depended on stderr-suppression continue to pass; one new test
demonstrates that overriding `stderr` actually captures bytes.

This bug was surfaced during a downstream consumer's
`auto_explain`-based profiling work (see Background below). The
consumer worked around it by configuring PostgreSQL's own
`logging_collector` to write to disk — an awkward dance that this fix
makes unnecessary.

### Background

The downstream consumer is the Kiroku event store project at
`/Users/shinzui/Keikaku/bokuno/kiroku-project/kiroku/`. Its
`docs/plans/26-postgresql-side-append-profiling-with-explain-analyze-and-auto-explain.md`
("EP-2" under
`docs/masterplans/3-append-performance-profiling-and-experiment-tracking-methodology.md`)
proposed capturing `auto_explain` output by exactly the pattern
above:

```haskell
h <- openFile "auto-explain.log" WriteMode
let cfg = Pg.defaultConfig
        <> Pg.autoExplainConfig 0
        <> mempty { PgC.stderr = Last (Just (Just h)) }
Pg.withConfig cfg action
```

Implementing that plan revealed the present bug. The Kiroku harness
routed around it via `logging_collector = 'on'` and
`log_destination = 'csvlog'`, which writes a CSV log directly to disk
through a different code path inside postgres. The workaround works
but doubles the complexity of every consumer that wants postgres's
stderr — they all have to configure the logging collector instead of
using the `Config.stderr` API that already exists. Fixing this plan's
target API removes that workaround for every future consumer.


## Progress

Use a checklist to summarize granular steps. Every stopping point must be documented here,
even if it requires splitting a partially completed task into two ("done" vs. "remaining").
This section must always reflect the actual current state of the work.

- [ ] Define a small `resolveStream :: Last (Maybe Handle) -> StreamSpec anyType ()` helper in `EphemeralPg.Process.Postgres` (or a sibling Internal module) that maps the three `Last (Maybe Handle)` shapes to the corresponding `typed-process` `StreamSpec`.
- [ ] Replace the two hardcoded `setStdout nullStream` / `setStderr nullStream` calls inside `startPostgres` (around `src/EphemeralPg/Process/Postgres.hs:77-78`) with `setStdout (resolveStream config.stdout)` and `setStderr (resolveStream config.stderr)`.
- [ ] Leave the `pg_isready` polling block's `setStdout nullStream` / `setStderr nullStream` (around `src/EphemeralPg/Process/Postgres.hs:180-182`) unchanged; that is an internal probe, not user-facing output.
- [ ] Add a unit or integration test under `test/` that opens a pipe (or temp file), sets `Config.stderr` to its write end, starts a cluster, issues a `RAISE LOG` (or just relies on cluster startup messages at `log_min_messages = 'log'`), shuts down, and asserts the captured bytes are non-empty.
- [ ] Update Haddocks on `Config.stdout` and `Config.stderr` to remove the "Nothing = discard" oversimplification and document the three-way semantics implemented by `resolveStream`.
- [ ] Add a `CHANGELOG.md` entry naming this as a bug fix, not a feature, and include a migration note for any caller that was relying on the prior (silent-discard) behaviour via the explicit `Last (Just (Just h))` form. (Callers using `Last Nothing` or `Last (Just Nothing)` are unaffected.)


## Surprises & Discoveries

Document unexpected behaviors, bugs, optimizations, or insights discovered during
implementation. Provide concise evidence.

(None yet.)


## Decision Log

Record every decision made while working on the plan.

- Decision: Treat this as a bug fix, not a behaviour change.
  Rationale: The field is documented as the implementation this plan ships ("Handle for postgres stdout (Nothing = discard)"), and there is no plausible caller relying on the documented behaviour being a no-op. Callers using the default `Last (Just Nothing)` see no change; the only callers affected by the fix are those who passed `Last (Just (Just h))` and silently got no output — which by definition cannot have been intentional.
  Date: 2026-05-19

- Decision: Scope the fix to the user-facing postgres server process only; leave the `pg_isready` probe's `setStdout nullStream` / `setStderr nullStream` (the second occurrence inside `Postgres.hs`) unchanged.
  Rationale: `pg_isready` is run by `waitForPostgres` as an internal liveness probe, typically a few times per cluster startup. Its output is implementation noise that callers do not want regardless of what they set on `Config`. Wiring it through would couple an internal mechanism to a user-visible field with no upside.
  Date: 2026-05-19

- Decision: Map `Last Nothing` (the field was not set anywhere in the `Semigroup` fold) to `nullStream`, matching the current default behaviour.
  Rationale: `defaultConfig` sets `stdout = Last (Just Nothing)` and `stderr = Last (Just Nothing)`, so under normal use the `Last Nothing` branch is unreachable. It is still well-typed and a future caller who constructs a `Config` from `mempty` alone (no `defaultConfig` mappend) should not crash. Choosing `nullStream` over `inherit` keeps the discard-by-default contract that downstream tests rely on.
  Date: 2026-05-19

- Decision: Do not change the type of `Config.stdout` / `Config.stderr` (keep `Last (Maybe Handle)`).
  Rationale: The three-way `Last (Maybe Handle)` semantics implemented by `resolveStream` is exactly what the field promises today; a richer `StreamSpec`-based type would be more expressive but would break every caller's `mempty { stderr = ... }` form and propagate `typed-process` types into the public API. The minimal-change fix is what this plan ships.
  Date: 2026-05-19


## Outcomes & Retrospective

Summarize outcomes, gaps, and lessons learned at major milestones or at completion.
Compare the result against the original purpose.

(To be filled during and after implementation.)


## Context and Orientation

The repository root is `/Users/shinzui/Keikaku/bokuno/ephemeral-pg-project/ephemeral-pg/`. Throughout this plan paths are relative to that root.

`ephemeral-pg` is a Haskell library that spins up disposable PostgreSQL clusters for tests and short-lived programs. The public module `EphemeralPg` (`src/EphemeralPg.hs`) exposes the lifecycle entry points (`with`, `withConfig`, `withCached`, `start`, `stop`) plus the `Config` record (re-exported from `EphemeralPg.Config`). A typical use looks like:

```haskell
import EphemeralPg qualified as Pg
main = do
  result <- Pg.withConfig Pg.defaultConfig $ \db -> do
    -- use db.connectionString to connect
    pure ()
  ...
```

The `Config` record lives at `src/EphemeralPg/Config.hs:62-95`. Two of its fields are relevant here:

```haskell
data Config = Config
  { ...
    stdout :: Last (Maybe Handle),  -- | Handle for postgres stdout (Nothing = discard).
    stderr :: Last (Maybe Handle)   -- | Handle for postgres stderr (Nothing = discard).
  }
```

The `Last (Maybe Handle)` wrapping is the same pattern the rest of the record uses, so combinations via `Config`'s `Semigroup` instance work as expected: the right-most non-`Last Nothing` wins. `defaultConfig` (`src/EphemeralPg/Config.hs:208-226`) sets both fields to `Last (Just Nothing)` with the trailing comment `-- Discard by default`.

The postgres server is launched by `startPostgres` (`src/EphemeralPg/Process/Postgres.hs:55-126`). The startup config built there is:

```haskell
let processConfig =
      proc postgresPath (map T.unpack args)
        & setStdin nullStream
        & setStdout nullStream     -- <-- ignores config.stdout
        & setStderr nullStream     -- <-- ignores config.stderr
        & setCreateGroup True
```

`setStdout`, `setStderr`, and `nullStream` come from `System.Process.Typed` (the `typed-process` package, on Hackage). `setStdout :: StreamSpec 'STOutput a -> ProcessConfig stdin stdoutOld stderr -> ProcessConfig stdin a stderr`; `nullStream :: StreamSpec anyStreamType ()` returns a stream-spec that discards (for output) or feeds nothing (for input). A handle can be wired into a `StreamSpec` with `useHandleOpen :: Handle -> StreamSpec anyType ()` (the handle stays open after the process ends) or `useHandleClose :: Handle -> StreamSpec anyType ()` (the handle is closed when the process exits). For the present use case `useHandleOpen` is the right choice: the caller owns the handle and decides when to close it.

The second occurrence of `setStdout nullStream` / `setStderr nullStream` inside `Postgres.hs` is in the `pg_isready` polling loop (`src/EphemeralPg/Process/Postgres.hs:175-182`). That occurrence stays untouched (see Decision Log).

Other process-launching code paths in the library that may seem related but are not in scope of this plan:

- `src/EphemeralPg/Process.hs:32-47` uses `readProcess` to run `initdb`, `createdb`, and similar one-shot helpers and captures both streams into the returned `StartError`/`StopError`. Those code paths already give the caller access to stderr via the error sum types; no change is needed.
- `src/EphemeralPg/Internal/CopyOnWrite.hs:89,97` discards `cp`'s stderr. That is an internal copy implementation detail and is not user-tunable.

The MasterPlan that motivated discovering this bug is in a different repository (Kiroku event store) at `docs/masterplans/3-append-performance-profiling-and-experiment-tracking-methodology.md`. The relevant child plan is `docs/plans/26-...md` (EP-2 in that initiative). Both files are paths in the Kiroku repo, not this repo; they are referenced here only as the historical record of where the bug was first observed. This plan stands on its own.


## Plan of Work

Two milestones. The first is the actual fix: replace the hardcoded `nullStream` calls with a helper that respects `config.stdout` / `config.stderr`. The second is the demonstration: a new test that proves the fix works end-to-end by capturing real stderr bytes from a running postgres into a handle.

### Milestone 1: Wire `Config.stdout` and `Config.stderr` through to `startPostgres`

The scope is a focused source edit. After this milestone the library still passes its existing test suite, and a caller passing `mempty { stderr = Last (Just (Just h)) }` to `withConfig` sees postgres's stderr arrive on `h`.

The work splits across three small edits to one file. In
`src/EphemeralPg/Process/Postgres.hs`, add an import of `useHandleOpen` from `System.Process.Typed`:

```haskell
import System.Process.Typed
  ( nullStream,
    proc,
    runProcess,
    setCreateGroup,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    unsafeProcessHandle,
    useHandleOpen,     -- <-- new
    waitExitCode,
  )
```

`useHandleOpen` is exported by the same module already imported here, so no new build-depends are needed (`typed-process` is already in the cabal stanza).

Add a small private helper at the bottom of `EphemeralPg.Process.Postgres` (or, equivalently, in a new `EphemeralPg.Internal.Process.StreamSpec` module if the maintainers prefer a separate home; the present plan assumes the helper lives where it is used and so adds it as a top-level binding in `Postgres.hs`):

```haskell
-- | Resolve a 'Config' output-handle field to a 'typed-process' 'StreamSpec'.
--
-- Three cases, matching the documented semantics of 'Config.stdout' and
-- 'Config.stderr':
--
-- * @Last Nothing@ — field was never set; default to 'nullStream' (discard).
--   Under normal use this branch is unreachable because 'defaultConfig'
--   sets both fields to @Last (Just Nothing)@; it is here so a caller
--   constructing a 'Config' from 'mempty' alone does not crash.
--
-- * @Last (Just Nothing)@ — explicitly discard. Returns 'nullStream'.
--
-- * @Last (Just (Just h))@ — write to the supplied handle. Returns
--   'useHandleOpen h'; the handle stays open after the process exits and
--   the caller is responsible for closing it.
resolveStream :: Last (Maybe Handle) -> StreamSpec anyStreamType ()
resolveStream (Last Nothing) = nullStream
resolveStream (Last (Just Nothing)) = nullStream
resolveStream (Last (Just (Just h))) = useHandleOpen h
```

This requires an `import System.IO (Handle)` at the top of the module (if not already present) and `import EphemeralPg.Config (Config (..))` already brings `Last`-typed access patterns into scope via the imported `Config` constructor.

Replace lines 77-78 of `src/EphemeralPg/Process/Postgres.hs`:

```haskell
-- before
& setStdout nullStream
& setStderr nullStream

-- after
& setStdout (resolveStream config.stdout)
& setStderr (resolveStream config.stderr)
```

The record-dot-syntax accessor `config.stdout` works because the module already uses `OverloadedRecordDot` (or has it enabled via `default-extensions` in the cabal common stanza). If not, the explicit form is `setStdout (resolveStream (stdout config))`; check the surrounding code to match the prevailing style.

The second occurrence of `setStdout nullStream` / `setStderr nullStream` inside `Postgres.hs` (the `pg_isready` polling block at around lines 180-182) is intentionally left as `nullStream`. See the Decision Log entry on that scoping.

Acceptance for Milestone 1: `cabal build` succeeds, `cabal test` passes (the existing test suite does not regress), and a hand-written one-line REPL check confirms the wire-up:

```haskell
import qualified EphemeralPg as Pg
import qualified EphemeralPg.Config as PgC
import Data.Monoid (Last (..))
import System.IO

main :: IO ()
main = do
  h <- openFile "/tmp/ephemeral-pg-stderr.log" WriteMode
  let cfg = PgC.defaultConfig
          <> (mempty :: PgC.Config) { PgC.stderr = Last (Just (Just h)) }
  _ <- Pg.withConfig cfg $ \_db -> pure ()
  hClose h
  bytes <- readFile "/tmp/ephemeral-pg-stderr.log"
  putStrLn ("captured " <> show (length bytes) <> " bytes; sample:")
  putStrLn (take 300 bytes)
```

Expected: a non-empty file containing at least the cluster's startup messages (e.g. "starting PostgreSQL", "listening on IPv4 address", "database system is ready to accept connections"). Today that file is empty.

### Milestone 2: Add a focused test that proves the fix works end-to-end

The scope is one new test module (or one new test case inside an existing module) under `test/`. After this milestone, `cabal test` exercises the new behaviour and would catch any regression in the wire-up.

The shape of the test:

1. Open a temp file in `WriteMode`.
2. Build a `Config` of `defaultConfig <> mempty { stderr = Last (Just (Just h)) }`.
3. Call `withConfig cfg $ \_db -> pure ()` and check the result is `Right ()`.
4. Close the handle.
5. Read the temp file's contents.
6. Assert the contents are non-empty (a stronger assertion such as
   "contains the substring `database system is ready`" is also reasonable
   and is friendlier to future debugging if the test ever fails).
7. Delete the temp file (in a `bracket` or `finally`).

The test does not need `auto_explain` or any custom GUC; the postgres cluster's own startup messages are enough. If the maintainers want a second, sharper test that demonstrates `auto_explain` integration specifically, that is a natural follow-up but is not required by this plan's acceptance.

Acceptance for Milestone 2: `cabal test` passes with the new test included; deliberately reverting the Milestone 1 source edit causes the new test to fail with an "expected non-empty stderr capture, got 0 bytes" message or equivalent.


## Concrete Steps

All commands run from the repository root `/Users/shinzui/Keikaku/bokuno/ephemeral-pg-project/ephemeral-pg/`.

### 1. Verify the bug exists today

```bash
cat > /tmp/ephemeral-pg-stderr-demo.hs <<'EOF'
import qualified EphemeralPg as Pg
import qualified EphemeralPg.Config as PgC
import Data.Monoid (Last (..))
import System.IO

main :: IO ()
main = do
  h <- openFile "/tmp/ephemeral-pg-stderr.log" WriteMode
  let cfg = PgC.defaultConfig
          <> (mempty :: PgC.Config) { PgC.stderr = Last (Just (Just h)) }
  _ <- Pg.withConfig cfg $ \_db -> pure ()
  hClose h
  bytes <- readFile "/tmp/ephemeral-pg-stderr.log"
  putStrLn ("captured " <> show (length bytes) <> " bytes")
EOF
cabal exec -- runghc /tmp/ephemeral-pg-stderr-demo.hs
```

Expected output before this plan lands: `captured 0 bytes`. The 0 is the bug.

### 2. Apply the fix

Edit `src/EphemeralPg/Process/Postgres.hs`:

- Add `useHandleOpen` to the `System.Process.Typed` import list.
- Add `import System.IO (Handle)` if it is not already present.
- Add the `resolveStream` helper at the bottom of the module (or in
  `EphemeralPg.Internal.Process.StreamSpec` as a sibling module — choose
  one and stay consistent).
- Replace `setStdout nullStream` (line 77) with
  `setStdout (resolveStream config.stdout)`.
- Replace `setStderr nullStream` (line 78) with
  `setStderr (resolveStream config.stderr)`.

Leave the `pg_isready` polling block at around line 181-182 untouched.

### 3. Build and run the existing tests

```bash
cabal build
cabal test
```

Expected: build succeeds, existing tests pass. If any existing test
fails that did not fail before, suspect that the test was relying on
the silent-discard behaviour with an explicit handle passed in; that
test is wrong and should be updated to assert the new behaviour.

### 4. Re-run the bug-demo from step 1

```bash
cabal exec -- runghc /tmp/ephemeral-pg-stderr-demo.hs
head -3 /tmp/ephemeral-pg-stderr.log
```

Expected: non-zero byte count, and `head -3` shows at least one line
that looks like PostgreSQL's startup log (for example, `starting
PostgreSQL 18.x on aarch64-apple-darwin25.3.0, compiled by …`).

### 5. Add the test from Milestone 2

Add the new test module under `test/`. The exact name and layout
should match whatever convention the rest of the test suite already
uses; consult `test/` and the `test-suite` stanza in
`ephemeral-pg.cabal` to align with the prevailing pattern.

```bash
cabal test
```

Expected: all tests pass, including the new one.

### 6. Update Haddocks and CHANGELOG

Edit `src/EphemeralPg/Config.hs` to replace the existing one-line
field comments on `stdout` and `stderr` with the three-way semantics:

```haskell
-- | Handle for postgres stdout.
--
--   * @Last Nothing@ — unset; falls back to 'nullStream' (discard).
--   * @Last (Just Nothing)@ — explicitly discard.
--   * @Last (Just (Just h))@ — write to the supplied 'Handle'. The
--     caller owns the handle and is responsible for closing it after
--     the cluster has stopped.
stdout :: Last (Maybe Handle),
-- (likewise for 'stderr')
```

Add a `CHANGELOG.md` entry under the next Unreleased version that
reads roughly:

```markdown
### Fixed

- `Config.stdout` and `Config.stderr` are now honoured by the postgres
  process. Previously the fields were accepted but their values were
  silently discarded because `EphemeralPg.Process.Postgres.startPostgres`
  hardcoded `setStderr nullStream` (and the equivalent for stdout). A
  caller passing a `Handle` will now see postgres's stdout/stderr bytes
  arrive on it; callers using the default (`Last (Just Nothing)`)
  continue to discard.
```

If the project uses a release-skill that owns CHANGELOG management
(e.g. via `.claude/skills/release/`), follow that workflow instead of
hand-editing.


## Validation and Acceptance

The plan is accepted when all of the following hold.

The first acceptance check is that the bug demo from "Concrete Steps
step 1" captures a non-zero number of bytes after the fix is in
place, and zero bytes immediately before. This is the user-visible
behaviour the plan exists to deliver.

The second check is that `cabal build` succeeds with no new warnings
introduced by the helper or the import line.

The third check is that `cabal test` passes both the existing tests
and the new test from Milestone 2. Reverting the Milestone 1 source
edit must cause the new test to fail; if it does not, the test is
not actually exercising the new code path and should be tightened.

The fourth check is that no existing caller has to change. A caller
who passes neither `stdout` nor `stderr` (or who uses
`defaultConfig` alone) sees the same discard behaviour as today.

The fifth check is that the public type of `Config` is unchanged
(`stdout :: Last (Maybe Handle)`, `stderr :: Last (Maybe Handle)`),
and the public API of `EphemeralPg` adds no new exports beyond
optional documentation/CHANGELOG churn. This is a fix, not a
behaviour change requiring a major version bump under PVP.


## Idempotence and Recovery

Every step in this plan is safe to repeat. The source edits in
Milestone 1 are pure substitutions; re-applying them is a no-op once
applied. The new test in Milestone 2 is idempotent against the test
runner (it creates and tears down its own temp file in a `bracket`
or `finally`).

If a later refactor moves `startPostgres` or splits `Postgres.hs`,
the `resolveStream` helper is small enough to relocate as a unit;
its only dependency is on `Last`, `Maybe`, `Handle`, and the
`StreamSpec` type from `typed-process`. No state, no IO.

If the test in Milestone 2 ever flakes (for example, because the
cluster started so quickly that no stderr bytes were emitted before
the process was sampled), the right recovery is to issue a single
explicit `RAISE LOG 'ephemeral-pg-test'` via the bench's database
handle inside the `withConfig` action, which forces a deterministic
line into the captured stream. This is documented here so a future
maintainer who sees an empty-capture failure does not assume the
fix has regressed.


## Interfaces and Dependencies

No new build-depends. The `typed-process` package is already in the
`ephemeral-pg.cabal` library stanza (used elsewhere in the same
module). `useHandleOpen` is exported by `System.Process.Typed` in
all `typed-process` versions the library currently supports.

Public-API surface at the end of the plan:

- `EphemeralPg.Config.Config.stdout :: Last (Maybe Handle)` — unchanged type, new effective behaviour.
- `EphemeralPg.Config.Config.stderr :: Last (Maybe Handle)` — unchanged type, new effective behaviour.

Internal helpers added (not exported from the library's public
surface unless the maintainer chooses to):

- `EphemeralPg.Process.Postgres.resolveStream :: Last (Maybe Handle) -> StreamSpec anyType ()` — local helper that maps a `Config` output-handle field to a `typed-process` `StreamSpec`. Lives where it is used.

Modules touched by this plan:

- `src/EphemeralPg/Process/Postgres.hs` — the implementation fix and the helper.
- `src/EphemeralPg/Config.hs` — Haddock-only update to the two field comments. Optional but recommended.
- `test/<NewModule>.hs` — the new test from Milestone 2; exact path matches the test suite's existing layout.
- `CHANGELOG.md` — bug-fix entry under the next Unreleased version.
