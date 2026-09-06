---
id: 3
slug: reap-stale-postgresql-instances-at-startup
title: "Reap stale PostgreSQL instances at startup"
kind: exec-plan
created_at: 2026-09-06T15:01:16Z
---

# Reap stale PostgreSQL instances at startup


This ExecPlan is a living document. Keep Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective current during implementation. Distill durable decisions into `docs/adr/` before completion.

## Purpose / Big Picture


Address [GitHub issue #1](https://github.com/shinzui/ephemeral-pg/issues/1): when a test consumer is killed without running cleanup, a later invocation should stop its abandoned PostgreSQL server and reclaim its temporary data directory. An explicit `sweepStaleInstances` operation and a default-enabled startup sweep will provide this behavior. Concurrent consumers, permanent directories, and reusable initialization caches must remain usable and unchanged.

The decisive demonstration is a subprocess test: start a database in a child consumer, kill only that consumer with `SIGKILL`, then start a new consumer in the same temporary root. The abandoned server must stop and its data directory disappear, while a second, live consumer remains connectable. Cleanup occurs on the next sweep, not immediately at the moment of the kill.

## Progress


- [x] (2026-09-06 15:51Z) Read plan and skill contracts; confirmed clean working tree, macOS toolchain, and filelock 0.1.1.9 release/source. Started baseline checks.
- [x] (2026-09-06 16:07Z) Milestone 1 safety gate: same-process and subprocess lock exclusion, SIGKILL release, directory replacement, and real postmaster inspection passed on macOS; the Linux Docker probe passed with filelock 0.1.1.9.
- [x] (2026-09-06 16:07Z) Implemented internal explicit sweep; 10 focused macOS examples pass, covering orphan recovery, concurrency, malformed state, cancellation and replacement.
- [x] (2026-09-06 16:13Z) Legacy, timeout, PID-reuse, opt-out, cache fallback and survivor-connection fixtures passed; enabled and exported automatic and explicit sweeping.
- [x] (2026-09-06 16:18Z) Startup integration, public documentation and ADR written. Main suite passed 38 examples on both macOS and Linux; all macOS OpenTelemetry suites and Haddock generation passed.
- [x] (2026-09-06 16:28Z) Deterministic initdb, copy and createdb cancellation barriers pass. Final main library suite: 43 examples, zero failures on macOS and Linux. Both macOS OpenTelemetry suites pass (four examples and one example).
- [x] (2026-09-06 16:31Z) Final build, regenerated API documentation, formatting and whitespace validation passed; verified both new public symbols in generated HTML and completed ADR distillation and plan closeout.
- [x] (2026-09-06 16:45Z) Replaced Docker fixtures with Apple containers and the pinned Nix test shell. All 48 examples passed on ARM64 Linux and macOS. Validated launcher cache reuse, concurrent-run rejection, pinned-image bootstrap, and shutdown after success and deliberate Cabal failure.

## Surprises & Discoveries


The baseline macOS suite passed with 11 examples and zero failures. The first orphan fixture exposed `/tmp` versus `/private/tmp` differences; startup and tests now canonicalize roots before allocating. Other local postmasters use relative paths or environment-based data paths, so command-line enumeration alone cannot exclude them. The adapter additionally observes each PostgreSQL process's working directory (batched `lsof` on macOS, `/proc/<pid>/cwd` on Linux). Disappearing shutdown workers require bounded re-observation rather than immediate success or deletion.

The original Linux Docker fixture supplied GHC 9.6.6 and PostgreSQL 17.11; the probe explicitly built filelock 0.1.1.9 because Debian's packaged version is 0.1.1.7. Its real orphan test passed (one example, zero failures). macOS uses GHC 9.12.4 and PostgreSQL 17.10. The Linux main library suite subsequently passed 38 examples with zero failures in 43.592 seconds using the full Docker fixture. The macOS main suite passed the same 38 examples in 134.5203 seconds. The probe remains distinct from full-suite evidence. The final cancellation validation is recorded in Outcomes & Retrospective.

The Nix bootstrap image has no `/bin/ps`. Linux inspection now resolves `ps` through `PATH`, with procps supplied by `devShells.test`. The shell uses the same GHC 9.12.4 and PostgreSQL 17.10 pins as local development, without editor tools, hook installation or development-database initialization.

## Decision Log


Decision (2026-09-06): expose `sweepStaleInstances :: Config -> IO [FilePath]`, and add `sweepStaleOnStart :: Last Bool` to `Config`, resolving an absent value to `True`. The issue's `CacheConfig` signature is a suggestion; the actual temporary root belongs to `Config`, whereas `CacheConfig.root` names the persistent template cache. Explicit sweeps run regardless of the automatic-sweep flag.

Decision (2026-09-06): protect newly created instances with an operating-system file lock held by the consumer for the whole instance lifetime. A file lock grants exclusive access while its file descriptor remains open, and is released by the operating system when the holder dies. Keep this lock outside the PostgreSQL data directory because cache restore and snapshot restore replace that directory. Mere absence of `postmaster.pid` does not establish that a directory is abandoned.

Decision (2026-09-06): include conservative legacy cleanup for existing untracked directories with valid `postmaster.pid` records. A dead recorded process permits cleanup only after excluding active users of that directory. A live legacy server additionally requires verified PostgreSQL identity and parent PID 1. Skip ambiguous records, unsupported process inspection, and untracked directories with no PID file. This intentionally leaves some old debris rather than guessing ownership. A parent PID of 1 is an adoption heuristic for legacy instances, not the ownership protocol for new ones.

Decision (2026-09-06): use bounded fast shutdown, never automatic `SIGKILL` escalation, and do not remove data while shutdown remains uncertain. Keep per-candidate ordinary I/O failures from failing unrelated database startup, but propagate asynchronous cancellation. Do not introduce process-global signal handlers, an always-running cleanup service, or cache eviction.

Decision (2026-09-06): keep scope to a plan in this create-mode session. No intention was supplied during research. Implementation, tests, documentation updates, and architectural records described below remain future work.

Decision (2026-09-06): persistent lifetime lock files are retained after retirement, while metadata is removed after successful cleanup. This avoids lock-inode recycling races; the cost is one small lock file per historical allocation. Registration and claims remain serialized by the persistent registry lock.

Decision (2026-09-06): consolidate cached and uncached initialization into one managed allocation. Cache restore failure reinitializes that same protected directory, eliminating duplicate sweeps and unprotected fallback allocations. The destructive-operation gate subsequently passed and automatic sweeping is now enabled.

Decision (2026-09-06): subprocess command output is captured in temporary files unlinked before launching, instead of pipes. Deterministic cancellation of initdb/createdb exposed pipe-reader cleanup waiting before child termination. Anonymous files retain lenient UTF-8 output capture, avoid that wait, and leave no output-file names after SIGKILL. Expected copy subprocess errors still permit fallback, but asynchronous exceptions propagate.

Decision (2026-09-06): normal cleanup rechecks that the data directory has no active PostgreSQL process. This prevents the pre-existing immutable snapshot/restart handle limitation from deleting a replacement server's live data. If observation remains uncertain, release the lease but retain metadata and data for a later dead-owner sweep.

Decision (2026-09-06): inspect an individual target with `ps -p <pid>`, separately from full process enumeration before deletion. The combined macOS suites exposed an unrelated server exiting during the original all-process identity lookup. Target-specific inspection removes this spurious uncertainty while preserving the independent full enumeration required to exclude active data-directory users.

Decision (2026-09-06): replace both Debian Dockerfiles and the standalone feasibility probe with `test/platform/apple-container.sh`. Apple containers supplies a local ARM64 Linux kernel; Nix supplies the locked project toolchain. Copy the current working tree into the Linux filesystem and run all Cabal suites as a non-root user. Retain the stopped container for Nix/Cabal caches. See [the Linux validation ADR](../adr/2-linux-tests-with-apple-containers-and-nix.md).

## Outcomes & Retrospective


Implemented all three milestones: external lifetime ownership, conservative explicit reaping, and default-enabled startup integration. The public API exposes `sweepStaleInstances` and `sweepStaleOnStart`; cached fallback paths share one protected allocation. Real SIGKILL fixtures prove the postmaster survives its consumer, then explicit or automatic sweeping stops it and removes only its abandoned data. Live survivor connections and reusable cache contents remain usable.

The final Linux main suite passed 43 examples with zero failures in 34.7830 seconds (GHC 9.6.6, PostgreSQL 17.11, filelock 0.1.1.9). The final combined macOS run passed the same 43 examples in 90.4557 seconds (GHC 9.12.4, PostgreSQL 17.10), plus the four-example OpenTelemetry suite and its one-example demo. `cabal build all` passed. `cabal haddock all`, `nix fmt` and `git diff --check` passed. Generated HTML contains both new public symbols; Haddock reports nonfatal link/coverage warnings.

The Apple containers follow-up passed all three suites through `./test/platform/apple-container.sh` on ARM64 Linux: 43 main examples (34.0226 seconds), four OpenTelemetry examples (1.9962 seconds), and one demo example (0.4682 seconds), all with zero failures. GHC 9.12.4 and PostgreSQL 17.10 came from the unchanged `flake.lock`; procps was 4.0.6 and the bootstrap image used Nix 2.35.2. The macOS test shell also passed all 48 examples, with the main suite taking 113.5554 seconds. The launcher successfully reused dependency caches and stopped the container after both success and a deliberate invalid-Cabal-option failure. A concurrent invocation was rejected without interrupting the active run. A fresh pinned-image bootstrap also passed. Both Dockerfiles and the redundant standalone probe are removed.

Durable decisions are distilled into [the ownership and reaping ADR](../adr/1-stale-instance-ownership-and-reaping.md). Important lessons were canonical path agreement, keeping control files outside replaceable data, separating target identity from global enumeration, and avoiding output-pipe cleanup blocking cancellation. Conservative limits remain intentional: unsupported/uncertain observations, legacy directories without PID files, active initialization children, and shutdown timeouts retain data. Persistent lock files prevent inode-recycling races. The existing snapshot API's process-handle redesign remains outside this plan; normal cleanup now refuses to remove its replacement server's live data.

## Context and Orientation


`src/EphemeralPg.hs` implements the public lifecycle. `start` resolves temporary or permanent data and socket directories, runs `initdb`, starts PostgreSQL, creates the requested database, and returns a `Database`. `stop` calls `stopPostgres`, then the handle's cleanup action. `restart` preserves that cleanup action while replacing the process handle. The `with` family arranges cleanup when Haskell code can unwind, which cannot cover `SIGKILL`.

`start` and `startCached` now share `startManaged`, which performs one optional sweep and then allocates and owns resources. `initialize` handles cache-disabled and key-error fallbacks, cold cache creation, warm restore, and restore failure within that same allocation. A restore failure reinitializes the claimed path instead of calling public startup recursively. `DirectoryPermanent` cached requests use ordinary initialization, retaining the permanent-directory contract without enrolling user-owned data.

`src/EphemeralPg/Internal/Directory.hs` uses `createTempDirectory` with the literal prefix `ephpg-data-`, not a hard-coded double-hyphen pattern. Scan immediate children whose names begin with that prefix, which also covers the `ephpg-data--*` examples in the issue. Socket directories use `pg-`; snapshots use `ephpg-snap-`. A temporary root comes from `getLast config.temporaryRoot`, otherwise `getTemporaryDirectory`.

`src/EphemeralPg/Internal/Cache.hs` separates `CacheConfig.root` from the temporary root and otherwise uses the XDG cache directory `ephemeral-pg`. Its `cleanupRuntimeFiles` removes copied `postmaster.pid` and `postmaster.opts`; it is not a liveness check and must never be called on sweep candidates before inspection. `createCache` copies initialized data before server startup. Ownership records outside the data directory will not enter these copies.

`src/EphemeralPg/Database.hs` stores cleanup as an `IO ()` closure. Capture the ownership-lock resource in this closure rather than adding a required field to the exported `Database` constructor. `src/EphemeralPg/Snapshot.hs` stops and restarts servers and replaces the data directory during restore; the external ownership lock must remain held across those operations. Its existing process-handle limitations are outside this change, but must not undermine sweep protection.

`src/EphemeralPg/Process/Postgres.hs` launches the postmaster, PostgreSQL's supervising process, in a new process group. Its `stopPostgres` requires a typed-process handle and waits on that child; a later sweeper does not have that handle and cannot reuse that waiting path. A PID is a numeric operating-system process identifier and can be reused after a process exits. Treat a PID as a lookup key, not proof of identity. Permission errors during a liveness probe mean unknown, not dead.

`ephemeral-pg.cabal` declares the library and `ephemeral-pg-test`; the test component also compiles modules directly from `src`, so new internal modules and dependencies must be listed in both components. `test/Main.hs` uses Hspec, QuickCheck, and real PostgreSQL. `cabal.project` includes `ephemeral-pg-opentelemetry/`; run its tests too. `nix/haskell.nix` supplies a GHC 9.12.4 development shell with PostgreSQL. The default shell initializes a repository-local `db` directory; do not sweep it. The `test` shell skips that initialization and supplies Linux procps explicitly. `test/platform/apple-container.sh` uses this shell inside Apple containers and runs every suite from a fresh source snapshot on the Linux filesystem. Supported target platforms for this plan are macOS and Linux on local filesystems.

Initial ADR discovery found no `docs/adr/` directory and no relevant ADRs. Implementation has now created [the ownership and reaping ADR](../adr/1-stale-instance-ownership-and-reaping.md). `mori show --full` reports no OKF bundles, so there is no profiled ADR format to enforce. During implementation create an ordinary Markdown architectural record at `docs/adr/1-stale-instance-ownership-and-reaping.md`, after rechecking for newly added conventions, explaining ownership, legacy exclusions, and the shutdown-before-deletion rule.

## Plan of Work


### Milestone 1: prove ownership and process inspection independently


Create `src/EphemeralPg/Internal/Instance.hs` for ownership and `src/EphemeralPg/Internal/ProcessIdentity.hs` for process inspection. Add focused tests in `test/StaleInstances.hs`, wired into `test/Main.hs` and the Cabal test component. First prove the dangerous assumptions without enabling automatic deletion: a held lock excludes another thread in the same process and a separate process; killing its holder releases it even while an exec-launched PostgreSQL child continues; lock ownership survives data-directory replacement.

Use `filelock` with an exclusive, nonblocking acquisition for sweeps. Put control files in a private, current-user-owned directory `.ephemeral-pg-instances-<uid>` immediately below the resolved temporary root. Use a persistent registry lock file there to serialize instance registration, record retirement, and sweep claims. Never unlink that registry lock during normal operation. Allocate a data directory and publish its record while holding the registry lock; acquire its separate lifetime lock before starting `initdb` or restoring data. Atomically publish a versioned metadata file containing the absolute data path, owner PID, and whether data is library-temporary. Keep metadata and lock files separate. Record temporary socket information only if needed for normal lifecycle cleanup; do not give this sweep authority to remove socket directories.

Release the registry lock before long startup, shutdown, or copy operations. Sweeps use nonblocking lifetime-lock acquisition and revalidate the record after acquiring it. Registration and retirement must use the same lock order: registry lock first, lifetime lock only nonblocking while the registry lock is held. Hold a successful candidate's lifetime lock through inspection and removal, and reacquire the registry lock to retire its record. Do not remove/recreate a lock pathname while another operation can still claim the old file; serialize retirement and revalidate record identity after opening. Treat malformed or symlinked registry state as unknown. Failed registration before any PostgreSQL work returns a `StartError` using the existing resource-error structure, instead of silently launching an unprotected tracked instance.

Implement process inspection as a small platform adapter returning `Gone`, `Present` with PID, parent PID, effective owner, command identity, start identity and data-directory association, or `Unknown` with a reason. Use argument-vector process execution, never shell interpolation. Prototype Linux process metadata and macOS `ps` inspection, normalize locale/timezone if parsing `ps`, and include paths with spaces. Verify the actual platform fields before selecting parsing code. A command name alone or successful signal-zero probe is insufficient. Require agreement between the candidate's canonical path, PostgreSQL invocation, and the PID file's data path and start information. If the platform cannot establish agreement, return `Unknown`. Inspect all same-user PostgreSQL processes when necessary to exclude another process actively using a candidate whose recorded PID is dead or missing. Failure to enumerate reliably must prevent deletion.

This milestone is explicitly a feasibility gate: record the exact process-inspection commands, representative outputs, and lock behavior on both target platforms. Promote only a tested adapter; unsupported inspection must skip candidates rather than broaden matching. Keep operating-system calls behind a small injectable interface so tests can force PID reuse, permission failures, and identity changes. Record the remaining narrow race between a final process check and sending a POSIX signal; do not claim atomic process targeting when the platform does not provide it.

Run the focused `Stale instances` tests described below. Acceptance is demonstrated lock exclusion/release, correct classification of live, dead and uncertain processes, and no signals or deletions in this milestone.

### Milestone 2: implement the explicit conservative sweep


Create `src/EphemeralPg/Internal/Sweep.hs` and export `sweepStaleInstances` through `src/EphemeralPg.hs`. Enumerate only immediate `ephpg-data-` children of the resolved temporary root. Resolve the root once; do not recurse to discover candidates. Require real directories owned by the current user, reject symlinks and unexpected control-file types, and verify the directory identity again before a destructive operation. Exclude the configured permanent data path, registry directory, snapshots, sockets, and all cache locations by construction. If filesystem access cannot establish these constraints, skip the candidate.

Parse `postmaster.pid` strictly as bounded input, including a positive PID greater than 1, recorded data path, and server-start information. Reject negative PIDs (which can represent a standalone PostgreSQL backend), malformed or truncated records, path mismatches, and changed records. Never signal process groups. For tracked instances, a held lifetime lock means an active owner even when the PID file or data directory is absent during startup or restore. An acquired lifetime lock permits inspection, not immediate deletion. If the recorded owner PID is still live, conservatively skip as well; this handles unusual inherited-handle or manually managed lifecycle situations.

For an unlocked tracked instance with a dead owner, stop a positively identified surviving postmaster, or remove the data only after confirming no process still uses it. This also covers initialization failures before a PID file exists, but requires excluding an orphaned `initdb` or other initialization child before removal. For legacy entries with no record, require a valid PID file and verified cluster identity; a live matching postmaster must have parent PID 1. Register a temporary sweep claim under the registry lock to exclude competing sweepers. Skip missing legacy PID files, non-orphan live servers, reused PIDs, uncertain parentage, and unprovable ownership. Do not use age as proof.

For a positively identified abandoned server, re-read both PID file and process identity immediately before sending `SIGINT` to that one PID. PostgreSQL fast shutdown ends active sessions and waits for its subprocesses to finish. Poll with a monotonic deadline of five seconds; do not call `waitExitCode` on an unrelated process or treat a timeout as success. Recheck that the original process is gone and no replacement process is using the directory, then remove data. Leave timed-out or unverifiable instances intact for a future sweep. Do not escalate to `SIGKILL`. These choices follow PostgreSQL's documented shutdown behavior: [server shutdown](https://www.postgresql.org/docs/current/server-shutdown.html).

Handle ordinary `IOException`s per candidate and continue to other entries; a missing/unreadable root returns an empty list. Do not catch `SomeException` indiscriminately or swallow cancellation. Use a bounded deletion helper that reports success, rather than the existing helper that discards errors. Return only paths actually removed by this call, sorted for stable results. An internal detailed outcome type should distinguish removed, active, uncertain, timed out, and failed candidates for tests; the public result stays the issue's simple list shape. No logging framework or telemetry redesign is required.

Acceptance is focused tests proving one abandoned fixture is removed, its second sweep returns `[]`, concurrent sweeps report each removal at most once, and every protected/uncertain fixture remains intact. Include a real legacy orphan, not only simulated process records.

### Milestone 3: integrate startup, prove SIGKILL recovery, and document the contract


Add the configuration field and its right-biased `Semigroup` combination in `src/EphemeralPg/Config.hs`; `mempty` carries `Last Nothing`, and `defaultConfig` enables the behavior. Refactor `src/EphemeralPg.hs` so public `start` and `startCached` perform at most one automatic sweep per invocation before allocating new resources. Internal fallbacks use an already-swept entry point, avoiding duplicate scans. Protect temporary instances with lifetime locks even when automatic sweeping is disabled, so another process can still clean them later.

Thread ownership through the shared `startManaged` allocation and `initialize` cache branches used by both public startup variants. These replace the original `startAndCache`, `startFromCache`, and `continueStartup` duplication. Audit every `Left` path and asynchronous exception between allocation and publishing the `Database`. Mask the transfer of lock ownership into the cleanup closure, restore interruptibility for blocking work, and ensure failure stops any launched process before releasing ownership and removing resources. The closure must release locks exactly once and preserve enough metadata for later cleanup if ordinary deletion fails. Do not hold the registry lock during ordinary server lifetime. Preserve the ownership lock through `restart` and snapshot operations without copying it into cache or snapshot contents. Skip ownership registration for permanent data directories.

Add a child-consumer mode to `test/Main.hs`, dispatched before Hspec. The test driver launches its own test executable with a dedicated argument and isolated short temporary root, then waits for a pipe readiness message containing the child's data path and postmaster PID. Kill only the child consumer with `SIGKILL`; do not kill its process group. Use bounded handshakes and polling rather than arbitrary sleeps. Keep a second consumer alive and assert that it can connect after explicit sweeping and after automatic startup. Ensure test finalizers clean up only processes and directories created by that test, including on failure. Add deterministic startup barriers in the internal test seam to exercise pre-initdb, cache-copy, restart and restore windows while another process sweeps.

Update `README.md`, `CHANGELOG.md`, and Haddock comments with the explicit API, custom temporary roots, opt-out, delayed cleanup after abnormal exit, five-second per-server shutdown bound, legacy limitations, local-filesystem assumption, and the fact that permanent data, sockets, snapshots, and caches are excluded. Explain that a large backlog can add startup latency, and that an uninspectable or unresponsive server is skipped. No changes to issue state, publishing, or consumer repositories are needed. Create the ADR described above and distill the final tested contract into it.

Acceptance is the complete test matrix below on macOS and Linux, all project tests passing, and generated API documentation exposing the new function and field. Record platform evidence honestly; do not claim cross-platform completion based on one host.

## Concrete Steps


Run all commands from the repository root, the directory containing `ephemeral-pg.cabal`. The first commands capture a baseline; PostgreSQL must run as a non-root user.

```bash
nix develop
cabal build all
cabal test ephemeral-pg-test --test-show-details=direct
```

If an existing shell already supplies GHC, Cabal, `initdb`, `postgres`, `createdb`, and `pg_isready`, omit `nix develop`. Expected baseline: build succeeds and Hspec reports zero failures. Baseline tests were not run during plan creation. Record existing failures separately before attributing failures to this change.

Before adding dependencies, repeat Mori discovery and authoritative release checks; the local registry may gain sources after this plan was written.

```bash
mori registry list
mori registry search filelock
curl -fsSL https://hackage.haskell.org/package/filelock/preferred.json
curl -fsSL https://hackage.haskell.org/package/filelock/filelock.cabal
git ls-remote --tags https://github.com/haskell-pkg-janitors/filelock.git
```

If Mori now finds a source owner, run `mori registry show <qualified-project> --full` and `mori registry docs <qualified-project>`, and inspect the returned local source. Do the same discovery for any additional dependency needed by process inspection; verify released versions before selecting bounds. Never inspect `/nix/store` for sources.

After each milestone, use this focused command. Name the Hspec group exactly `Stale instances` so it selects the intended cases.

```bash
cabal test ephemeral-pg-test --test-show-details=direct --test-options='--match "Stale instances"'
```

Expected: the milestone's cases are actually listed, zero failures, and a nonzero example count. The final recovery cases should include descriptions equivalent to the following; capture actual output in the living sections rather than recording this expected transcript as a test result.

```text
Stale instances
  releases ownership after a consumer is SIGKILLed
  reaps an orphan and preserves a live consumer
  sweeps once across cached startup fallbacks
  preserves cache contents and permanent directories
  leaves uncertain and timed-out instances intact
```

At completion run the project-wide checks:

```bash
cabal build all
cabal test all --test-show-details=direct
cabal haddock all
nix fmt
git diff --check
```

Review formatter changes and retain only changes belonging to this work. All test suites must pass; documentation generation and whitespace validation must succeed. On Apple silicon, run `./test/platform/apple-container.sh` to validate all suites on ARM64 Linux locally. Record OS, PostgreSQL version, compiler version and results. A separate Linux host can instead run `nix develop .#test -c cabal test all --test-show-details=direct`. If no Linux environment is available, explicitly record that remaining validation rather than inventing evidence.

## Validation and Acceptance


The end-to-end fixture must prove that killing the consumer leaves the postmaster alive before the sweep; otherwise the test has not reproduced the issue. After explicit sweep or default-enabled startup, its data directory is absent, its original postmaster no longer serves connections, and the returned removal list is correct. Repeat for uncached startup, first cached startup, warm cache restore, caching disabled, and cache-restore fallback. The survivor consumer must still connect throughout. With `sweepStaleOnStart = Last (Just False)`, startup must leave a stale fixture alone; an explicit sweep must still reap it.

Exercise concurrent owners in separate processes and in the same process; two simultaneous sweepers; live ownership during initialization, cache copy, restart, and snapshot replacement; startup errors before and after launching PostgreSQL; and asynchronous cancellation during ownership transfer. Cache fixtures should retain file contents and remain reusable, not merely retain their top-level directory. Add `Config` tests for identity, right-biased overrides and default resolution of the new field. Verify permanent data survives both startup variants, including a permanent path whose basename resembles a temporary instance.

Safety cases must cover unreadable paths, symlink candidates and control files, path replacement during inspection, another user's directory when the environment permits it, truncated PID files, negative/zero/one/overflow PIDs, PID reuse by an unrelated process, mismatched start identity, non-orphan legacy postmasters, permission-denied liveness probes, disappearing candidates, failed enumeration, stale metadata, orphaned initialization children, and shutdown timeout. Use injected platform results for rare races, plus real OS processes for the principal ownership and orphan tests. A skipped candidate must remain on disk and no signal should have been sent to an unrelated process. Legacy missing-PID directories remain untouched. Repeated calls must be harmless and must not report removals that failed.

The change is complete only when these observable behaviors, public documentation, and the ADR match. The scope does not promise automatic deletion of every historical directory or immediate cleanup without another invocation.

## Idempotence and Recovery


Every sweep takes fresh observations. An already removed directory is a no-op; an uncertain one remains eligible for a later call. Only retire a record after successful data removal or confirmed prior absence with no live owner. Keep incomplete shutdown metadata for retry. Never clear cache contents as a recovery step and never remove all prefix-matching directories with a shell command.

Tests must use a private temporary root with a short path suitable for macOS Unix sockets and a separate private cache root, never the user's ordinary temporary-directory backlog. Only deliberate test child processes receive `SIGKILL`. On test failure, finalizers should stop the known test server using its verified identity before removing test directories. A cleanup failure is evidence to preserve, not a reason to delete uncertain live data.

Implementation commits remain on the current branch unless the user requests another branch. Use Conventional Commits and include `ExecPlan: docs/plans/3-reap-stale-postgresql-instances-at-startup.md` as a trailer. If implementation is interrupted, update Progress with timestamped completed and remaining entries and leave automatic sweeping disabled in unfinished code until the safety gate passes.

## Interfaces and Dependencies


The public surface belongs in `src/EphemeralPg.hs` and `src/EphemeralPg/Config.hs`:

```haskell
sweepStaleInstances :: Config -> IO [FilePath]

-- New Config record field, combined using Last's existing semantics:
sweepStaleOnStart :: Last Bool
```

The internal ownership module should expose an opaque `InstanceLease`, acquisition for newly allocated temporary instances, and idempotent release/retirement operations. `Internal.ProcessIdentity` owns all OS-specific observations. `Internal.Sweep` owns candidate classification, bounded shutdown, deletion and reporting. Keep process inspection injectable for deterministic tests; do not expose a public function that signals arbitrary PIDs or deletes arbitrary paths.

Use `time >=1.12 && <1.17` for normalized `ps` date parsing; Mori located `mori://haskell/time/packages/time`, and Hackage plus upstream tags confirmed release 1.16.0.1. Use existing `directory`, `filepath`, `unix`, `process`, and `typed-process` dependencies for filesystem work, ownership checks, signals, and subprocesses after consulting Mori for their APIs. Add `filelock >=0.1.1.9 && <0.2` to the library and test component, subject to the repeated release check above. Hackage reported 0.1.1.9 during research and the current upstream tag `v0.1.1.9` resolved to commit `74e5cd6f8e3cf1ca72af118782f256887f5fd9ba`. The registry had no filelock source entry, so its released source was read directly from Hackage. This release uses `flock` and opens descriptors with close-on-exec on the repository's `unix >=2.8` range. Close-on-exec prevents the executed PostgreSQL program from retaining the consumer's lock. Still prove that behavior with the actual subprocess test.

Dependency ownership reference: `mori://haskell-pkg-janitors/filelock` (intended canonical project URI; no local registration found). Source artifact path: `System/FileLock/Internal/Flock.hsc`; artifact-level URI pending. The plan does not change existing dependency pins or bounds beyond the new locking dependency. No PostgreSQL extension, background daemon, consumer-installed signal handler, or OpenTelemetry API change is required.

Revision (2026-09-06): recorded platform feasibility evidence, canonical-root handling, working-directory inspection, persistent lock inode policy, and startup consolidation.

Revision (2026-09-06): recorded 38-example macOS/Linux suite evidence, enabled startup sweep after the destructive-operation gate, and documented the cancellation-driven output-capture and live-data cleanup changes.

Platform validation commands (repository root):

```bash
nix develop .#test -c cabal test all --test-show-details=direct
container system start
./test/platform/apple-container.sh
```

The Apple container script requires `container`, `git`, `jq` and `tar` on an Apple
silicon Mac. It uses the official Nix 2.35.2 image pinned by digest, transfers
tracked and non-ignored untracked working-tree files into a fresh Linux directory,
and realizes `path:$PWD#test` without rewriting `flake.lock`. The Nix setup runs as
root, then all Cabal builds and tests run as user `test` (UID 1000). No host build
outputs or database directories are shared. The main and both OpenTelemetry suites
run on Linux. The former standalone probe's assertions live in `test/StaleInstances.hs`.

The dedicated `ephemeral-pg-nix-validation` container stops on success or failure;
Nix and Cabal caches survive for subsequent runs. The script refuses to reuse a
running container. Failed source snapshots remain under `/work/source.*` inside
that container and build logs under `/home/test/dist-newstyle`. Re-run the same
command to retry. Use `EPHEMERAL_PG_CONTAINER` for a separate container. CPU and
memory defaults are four CPUs and 4 GiB, overridable at creation through
`EPHEMERAL_PG_CPUS` and `EPHEMERAL_PG_MEMORY`. Successful source snapshots are
removed. See the Linux validation ADR for the durable workflow decision.

Process observation uses the following argument vectors, without shell evaluation:

```text
/bin/ps -ww -p PID -o pid=,ppid=,uid=,stat=,lstart=,comm=
/bin/ps -ww -axo pid=,ppid=,uid=,stat=,lstart=,comm=
/usr/sbin/lsof -a -p PID_LIST -d cwd -Fn
```

The shown absolute `ps` paths apply to macOS; Linux uses the same arguments with `ps` resolved through `PATH`. `ps` runs with `LC_ALL=C` and `TZ=UTC`. Linux reads the selected `/proc/PID/exe`,
`cmdline`, and `cwd` entries. The successful Linux feasibility fixture observed a
postmaster with PID 23, parent 1, effective UID 100, start epoch 1788710796, command
`/usr/lib/postgresql/17/bin/postgres`, and data working directory
`/tmp/epg-67fbb65f997e520e/ephpg-data--7c9f9cb6f428e552`; it then received one fast
shutdown and the directory was removed. macOS fixtures verify the same fields
against each real PID file and assert live survivor connections.

Revision (2026-09-06): documented reproducible Linux validation, target-specific
process inspection, final shared startup structure, and the additional time dependency.

Final test evidence:

```text
macOS: cabal test all --test-show-details=direct
  ephemeral-pg-test: 43 examples, 0 failures (90.4557 seconds)
  ephemeral-pg-opentelemetry-test: 4 examples, 0 failures
  ephemeral-pg-opentelemetry-demo: 1 example, 0 failures
Linux: main library suite in the full Docker fixture, current source mounted read-only
  ephemeral-pg-test: 43 examples, 0 failures (34.7830 seconds)
cabal build all: passed
cabal haddock all: passed; both public symbols verified in generated HTML
nix fmt: passed
git diff --check: passed
```

Revision (2026-09-06): recorded final cross-platform recovery/cancellation results,
updated orientation to the implemented lifecycle, and distilled the final lessons
and intentional exclusions into the outcome and architectural record.

Revision (2026-09-06): completed final build/documentation/format verification and marked all milestones complete.

Revision (2026-09-06): replaced Docker validation instructions with Apple containers and the shared Nix test shell, recorded the Linux `ps` path portability fix, and added the Linux validation ADR. All 48 examples subsequently passed on macOS and ARM64 Linux through the Nix test shell; the checked-in launcher passed end-to-end validation and stopped the container on success and failure.


Apple containers migration evidence (2026-09-06):

```text
./test/platform/apple-container.sh
  Linux aarch64; GHC 9.12.4; PostgreSQL 17.10; procps-ng 4.0.6
  ephemeral-pg-test: 43 examples, 0 failures
  ephemeral-pg-opentelemetry-test: 4 examples, 0 failures
  ephemeral-pg-opentelemetry-demo: 1 example, 0 failures
  exit 0; container state stopped
nix develop .#test -c cabal test all --test-show-details=direct
  macOS: 43 + 4 + 1 examples, 0 failures
Concurrent launcher invocation: rejected, active run continued
Deliberately invalid Cabal option: nonzero exit, container state stopped
Fresh digest-pinned Nix image: Linux aarch64, Nix 2.35.2, test user UID 1000
bash -n test/platform/apple-container.sh test/platform/run-tests.sh: passed
nix fmt: passed
git diff --check: passed
```
