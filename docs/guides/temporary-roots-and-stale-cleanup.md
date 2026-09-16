---
type: Explanation
title: Temporary roots and stale cleanup
description: Why stale-instance sweeping is scoped to a single temporary root, and how an unstable $TMPDIR silently leaks abandoned clusters.
docId: DOC-4
tags: [ephemeral-pg, cleanup, temporary-root, tmpdir, operations]
generated:
  by: human:nadeem
  at: 2026-09-16T14:52:42Z
---

# Temporary roots and stale cleanup

`start` and `startCached` sweep abandoned PostgreSQL clusters before allocating a
new one, so a consumer killed with `SIGKILL` does not leak a postmaster forever.
The sweep is scoped to a single directory: the configured `temporaryRoot`, or the
system temporary directory when that is unset.

That scope is the part worth understanding. If the temporary root changes between
runs, the sweep never sees what earlier runs left behind, and abandoned clusters
accumulate indefinitely while the feature appears to be enabled.

## The default is `$TMPDIR`, which is not always stable

With `temporaryRoot` unset, the root comes from
`System.Directory.getTemporaryDirectory`, which returns `$TMPDIR` on POSIX and
falls back to `/tmp`. Several common environments give each session its own
`$TMPDIR`:

| Environment | `$TMPDIR` |
| --- | --- |
| `nix develop` / `nix-shell` | `/tmp/nix-shell.XXXXXX`, new per shell |
| systemd units with `PrivateTmp=yes` | private `/tmp` per service start |
| Some CI runners and container images | per-job or per-step directory |
| macOS launchd services | per-user `/var/folders/...` |

Under any of these, a test run allocates its clusters inside that session's
directory. When the run is killed, the postmaster survives and keeps that
directory alive. The next run gets a *different* `$TMPDIR`, sweeps an empty new
root, finds nothing, and leaks again.

The orphans are still perfectly reapable — they simply are not being looked at.
Nothing in the safety protocol rejects them.

### What this looks like

Four clusters abandoned by four separate `nix develop` sessions, each in its own
root, still running well over a day later:

```
PID 44741  /tmp/nix-shell.Ex1kKN/ephpg-data--81268893861081c0  port 62643
PID 45523  /tmp/nix-shell.Vwr5fv/ephpg-data--67042a9bec14c9a7  port 62675
PID 48818  /tmp/nix-shell.mofVo9/ephpg-data--6ae20a3606256f38  port 62710
PID 58308  /tmp/nix-shell.pPgbfp/ephpg-data--553fa4c95bdb49fe  port 62872
```

Every one had been reparented to PID 1 and held zero client connections — that is,
they satisfied the identity requirements a sweep imposes on a live untracked
postmaster. They survived only because no later session ever searched those roots.

## Set a stable root

Point `temporaryRoot` at a path that does not change between runs, and create it
before use. Key it by effective uid so that each user owns the root they write to
— see [Choosing the path](#choosing-the-path) for why:

```haskell
import Data.Monoid (Last (..))
import EphemeralPg qualified as Pg
import System.Directory (createDirectoryIfMissing)
import System.Posix.User (getEffectiveUserID)

testConfig :: IO Pg.Config
testConfig = do
  uid <- getEffectiveUserID
  let root = "/tmp/ephpg-my-project-" <> show uid
  createDirectoryIfMissing True root
  pure Pg.defaultConfig { Pg.temporaryRoot = Last (Just root) }
```

Then use the config-taking entry point rather than the zero-argument convenience
wrapper:

```haskell
config <- testConfig
result <- Pg.withCachedConfig config Pg.defaultCacheConfig $ \db ->
  -- Use the database...
```

`withCached` and `with` take no `Config`, so they always resolve to `$TMPDIR`.
Reach for `withCachedConfig` or `withConfig` as soon as you need a stable root.

### Choosing the path

- **Keep it short.** The socket directory lives under the same root, and
  `validateSocketPath` rejects anything that would push the Unix socket path past
  the platform limit. A deep root fails at startup rather than silently.
- **Key it by user.** The instance registry is namespaced as
  `.ephemeral-pg-instances-<uid>`, but the root directory holding it is not. A
  fixed path is created `0700` by whoever gets there first, and every other user
  is then locked out of it. See [When another user gets there first](#when-another-user-gets-there-first).
- **Do not point it at a permanent data directory.** Permanent data, sockets,
  snapshots and caches are excluded from sweeping by design.

### When another user gets there first

A stable root has to be stable *per user*, not merely constant. The usual way to
discover this is not a shared workstation but a build sandbox, which runs the
same suite as a different uid:

```text
Failed to start temp database: ResourceError
  (DirectoryCreationFailed "/private/tmp/ephpg-my-project"
    "/private/tmp/ephpg-my-project/.ephemeral-pg-instances-361:
     createDirectory: permission denied (Permission denied)")
```

A developer ran the suite first and owns `/tmp/ephpg-my-project` with mode `0700`.
The Nix build sandbox then runs as uid 361, cannot enter it, and the suite fails
at startup — on a machine where it passes interactively, which makes it look like
a sandbox problem rather than a path problem.

Put the uid in the path so each user gets a root they own:

```haskell
import System.Posix.User (getEffectiveUserID)

ephemeralRoot :: IO FilePath
ephemeralRoot = do
  uid <- getEffectiveUserID
  pure ("/tmp/ephpg-my-project-" <> show uid)
```

The uid keeps the root stable across that user's sessions, which is all the sweep
requires, while keeping users out of each other's directories. Prefer it to
`$USER`, which several sandbox users can share, and to `$HOME`, which a build
sandbox may point at an unwritable path.

## Sharing one root across suites

A shared root is safe for concurrent runs and is the intended arrangement. Live
instances hold exclusive lifetime locks, and a sweep claims only unlocked
candidates without blocking, so a sweep in one suite cannot reap a cluster another
suite is actively using. Multiple packages in the same project can and should
point at the same root — that is what lets a run of one suite clean up after a
killed run of another.

## Verifying it works

Cleanup after an abnormal exit is deferred to the *next* startup, so a sweep is
observable only on a subsequent run. To check the wiring end to end:

1. Start an instance, then `SIGKILL` the consumer process (not the postmaster).
2. Confirm the postmaster survives and its data directory is still present under
   the root.
3. Start a second instance from a **new shell session**.
4. Confirm the abandoned postmaster is gone and the directory was removed.

Step 3 is the one that matters: running it from the same shell passes even with a
per-session `$TMPDIR`, which is exactly the bug this document is about.

`sweepStaleInstances` performs the same sweep on demand and returns the sorted
canonical paths it actually removed, which makes it convenient as an assertion:

```haskell
removed <- Pg.sweepStaleInstances config
```

It honours `temporaryRoot` independently of `sweepStaleOnStart`, so it still works
when automatic sweeping is disabled.

## Related

- [ADR 1: Stale instance ownership and reaping](../adr/1-stale-instance-ownership-and-reaping.md)
  — the ownership, identity and signalling protocol.
- `README.md`, "Cleanup after a killed consumer" — the short version.
