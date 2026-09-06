# Linux tests with Apple containers and Nix

Status: Accepted
Date: 2026-09-06

## Context

Stale-instance recovery inspects operating-system processes and relies on local
filesystem locks. macOS tests cannot establish Linux `/proc` behavior. The original
Debian Docker fixtures duplicated dependencies with apt and a manual filelock
build, using a different GHC and PostgreSQL from the repository's Nix environment.

## Decision

Use Apple containers on Apple silicon to provide ARM64 Linux locally. Run the
complete Cabal project through `devShells.test`, which uses the same locked Nix
inputs and GHC version as development, without HLS, hook installation or a
persistent development database. The official Nix bootstrap image is pinned by
digest; it does not define the project toolchain. No project Dockerfile is needed.

Transfer the current source into a fresh directory on the container's Linux
filesystem. Run builds and PostgreSQL as an unprivileged user. Supply procps in
the Linux shell and resolve `ps` through `PATH` on Linux, because Nix installations
do not require `/bin/ps`. Keep macOS's system `ps` path.

Retain the dedicated container for dependency caches and stop it after each run
to release runtime resources. Reject reuse while it is running. Successful runs
remove their source snapshots; failures preserve snapshots and build logs for
diagnosis. The complete suites replace the standalone feasibility probe, whose
ownership, identity and orphan-recovery assertions are covered by
`test/StaleInstances.hs`.

## Consequences

`test/platform/apple-container.sh` is the repeatable local Linux validation entry
point. The host needs Apple containers, Git, jq and tar. Initial setup downloads
Linux dependencies; subsequent runs reuse the container's Nix and Cabal caches.
No host database directories or macOS build products enter the test environment.
Container memory and CPU limits are selected at creation. This establishes ARM64
Linux behavior; it does not establish x86-64 Linux compatibility.

The same test shell can run directly on macOS or a Linux host with
`nix develop .#test -c cabal test all --test-show-details=direct`.

## Evidence

On 2026-09-06, the checked-in launcher passed all 48 examples on ARM64 Linux
with GHC 9.12.4, PostgreSQL 17.10 and procps-ng 4.0.6. The same test shell passed
all 48 examples on macOS. Launcher validation covered a fresh digest-pinned image
bootstrap, dependency cache reuse, rejection of an already-running container, and
stopping the container after both success and a deliberate Cabal option error.
