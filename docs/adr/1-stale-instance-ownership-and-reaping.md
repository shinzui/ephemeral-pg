# Stale instance ownership and reaping

Status: Accepted
Date: 2026-09-06

## Context

A consumer killed with SIGKILL cannot run Haskell cleanup. Its PostgreSQL child
can survive, retaining a temporary cluster indefinitely. Absence of a PID file
is not evidence of abandonment: initialization and snapshot replacement both
have legitimate windows without that file. PostgreSQL PIDs can be reused.

## Decision

Register temporary data allocations under a private current-user registry in
the canonical temporary root. Serialize registration and claims with a persistent
registry lock. Each instance also holds an exclusive filelock lifetime lock,
acquired before initialization and retained in the Database cleanup closure.
Lifetime locks live outside data directories so copying, restart, and replacement
do not lose protection or copy ownership into a cache. Close-on-exec descriptors
prevent executed PostgreSQL children from retaining consumer ownership.

Retain lifetime lock inodes after retirement to prevent a waiting operation from
locking an obsolete inode. Remove metadata after successful data cleanup. This
costs one small persistent lock file per allocation. The registry is private to
the effective user and requires local filesystem locking semantics.

A sweep claims an unlocked candidate without blocking, then requires a dead owner
and verifiable process observations. It inspects only immediate `ephpg-data-`
directories owned by the effective user and rejects symlinks and malformed control
files. Target identity queries select one PID so unrelated process exits cannot invalidate
that observation. A separate full enumeration excludes active directory users.
macOS inspection combines locale-normalized `ps` metadata with batched
`lsof` working directories. Linux combines `ps` with `/proc` executable, argument,
and working-directory observations. An unsupported or failed observation is
uncertain and cannot authorize deletion.

Before signaling, require PID, effective user, executable identity, invocation,
working directory, PID-file data path, and start time to agree. The PID-file
wall-clock start and second-resolution process creation time may differ by up to
two seconds. Re-read identity and control state before sending SIGINT to that one
PID. There remains a narrow non-atomic POSIX process-check-to-signal race; this
protocol does not claim kernel-atomic targeting. Wait with a monotonic five-second
shutdown deadline and never escalate sweeps to SIGKILL. After shutdown, enumerate
processes again and recheck the directory inode before removal.

For historical untracked clusters require a valid PID file and PostgreSQL version
marker. A live legacy postmaster additionally needs parent PID 1. Missing legacy
PID files, unrelated/reused PIDs, active initialization children and uncertain
inspection are retained. No age threshold grants ownership.

## Consequences

`start` and `startCached` perform at most one default-enabled sweep; an absent
`sweepStaleOnStart` resolves to true. Explicit sweeps ignore the flag. Opted-out
instances still register ownership. Cleanup after abnormal exit is delayed until
a later sweep. Large backlogs add startup latency and uncertain candidates may
remain indefinitely.

One managed allocation handles ordinary initialization, cache hits, and cache
fallbacks. Permanent-directory cached requests use ordinary initialization.
Permanent data, reusable caches, sockets, and snapshots are outside sweep scope.
Ordinary failures skip candidates, while asynchronous cancellation propagates.
Command output uses temporary files unlinked before launch, avoiding pipe-reader
cleanup waiting ahead of child termination during cancellation and leaving no
output-file names after SIGKILL.
Normal cleanup also excludes active PostgreSQL processes because existing snapshot
operations can replace the process behind an immutable Database handle. Snapshot
handle redesign remains separate work.

## Evidence

The implementation plan records macOS GHC 9.12.4 / PostgreSQL 17.10 recovery tests
and the Linux GHC 9.6.6 / PostgreSQL 17.11 probe with filelock 0.1.1.9. The probes
kill only the consumer and verify the postmaster survives before reaping it.
The main safety suite covers PID reuse, timeout, concurrent claims, replacement,
malformed state, cancellation, cache fallbacks, live connections and legacy
recovery. See [the implementation plan](../plans/3-reap-stale-postgresql-instances-at-startup.md)
for final commands and results. The current Linux workflow uses Apple containers
and the pinned Nix test shell (GHC 9.12.4 / PostgreSQL 17.10), with all 43 main
examples and both OpenTelemetry suites passing. See [the Linux validation ADR](2-linux-tests-with-apple-containers-and-nix.md).
