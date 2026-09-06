---
name: release
description: Release a new version of the Haskell package following PVP
argument-hint: "[major|minor|patch]"
disable-model-invocation: true
allowed-tools: Read, Bash, Edit, Glob, Grep, Write
---

# Release Skill

Release a new version of the `ephemeral-pg` package to Hackage.

## Scope

`cabal.project` contains two packages:

- `ephemeral-pg` (root, `ephemeral-pg.cabal`) — published on Hackage. **This skill releases
  this package.**
- `ephemeral-pg-opentelemetry` (`ephemeral-pg-opentelemetry/`) — versioned independently and
  **not yet on Hackage** (the README says so). It is not part of this workflow. It has no
  `CHANGELOG.md` of its own, and its `license-file: ../LICENSE` points outside the package
  directory, so it is not sdist-ready. Releasing it is a separate, unsolved task.

Run everything from the repository root inside the Nix dev shell (direnv loads it via
`use flake`). Bare `cabal check`, `cabal sdist`, and `cabal haddock` target the package in
the current directory — i.e. `ephemeral-pg` — which is what we want. Adding `all` would
target both packages; do not.

## Arguments

`$ARGUMENTS` is an optional version bump hint: `major`, `minor`, or `patch`.
If omitted, determine the bump level from the changes (see step 2).

## Steps

### 1. Determine what changed since the last release

- Read the current version from `ephemeral-pg.cabal` (the `version:` field).
- Find the latest git tag matching `v*` (tags are `v<version>`, e.g. `v0.2.2.0`).
- Run `git log --oneline <last-tag>..HEAD` to list commits since the last release.
- Read the `## Unreleased` section of `CHANGELOG.md`. This is the curated record of what
  is shipping and is maintained as work lands — it is the primary input, not the commit log.
- If there are no commits since the last tag and no `## Unreleased` entries, inform the
  user there is nothing to release and stop.
- Confirm the working tree is clean (`git status --porcelain`). Do not release with
  uncommitted changes.

### 2. Determine the next version using PVP

The Haskell PVP version format is `A.B.C.D`:
- `A.B` is the **major** version — bump for breaking API changes (removed/renamed exports,
  changed types, changed semantics, new fields on exhaustively-constructed records)
- `C` is the **minor** version — bump for backwards-compatible API additions (new exports,
  new modules, new type class instances)
- `D` is the **patch** version — bump for bug fixes, documentation, internal-only changes,
  performance improvements

Rules:
- If `$ARGUMENTS` is `major`, `minor`, or `patch`, use that bump level.
- Otherwise, derive the bump from the `## Unreleased` entries and the commit log:
  - Breaking/removed/renamed exports, or a new field on a record consumers construct
    exhaustively → major
  - New exports, modules, or config fields that are purely additive → minor
  - Fixes, docs, refactors, internal changes → patch
- Present the proposed bump to the user and ask for confirmation before proceeding.

Increment the version:
- **major**: increment `B`, reset `C` and `D` to 0 (e.g. `0.2.2.0` → `0.3.0.0`)
- **minor**: increment `C`, reset `D` to 0 (e.g. `0.2.2.0` → `0.2.3.0`)
- **patch**: increment `D` (e.g. `0.2.2.0` → `0.2.2.1`)

If the bump is **major**, check `ephemeral-pg-opentelemetry.cabal`: its `ephemeral-pg`
dependency is currently unbounded, so nothing needs updating, but if a bound has been added
since, widen or move it to match.

### 3. Verify before you tag

Do this *before* creating any commit or tag — a failure here must not leave a pushed tag
for a release that cannot ship.

- `just sdist-check` (`cabal check`) — must report no errors or warnings.
- `just test` (`cabal test`) — the suite starts real PostgreSQL clusters and needs the Nix
  dev shell on PATH. For Linux verification, `test/platform/run-tests.sh` runs the suite
  under Apple containers with the pinned Nix test shell.
- If either fails, stop and report. Never skip them.

### 4. Update version and changelog

- Edit `ephemeral-pg.cabal` to set the new version.
- Edit `CHANGELOG.md`: rename the `## Unreleased` heading to `## <new-version>` and leave a
  fresh empty `## Unreleased` section above it for the next cycle.
  - Fold in anything that landed since the last release but was never written into
    `## Unreleased`.
  - Group entries under `### Breaking Changes`, `### New Features`, `### Bug Fixes`,
    `### Other Changes`, omitting empty categories. (Existing released sections use this
    shape; the `## Unreleased` section is often kept as plain bullets while work is in
    flight — organize it into categories at release time.)
- Show the user the changelog entry and version bump for review before committing.

### 5. Commit, tag, and push

- Stage `ephemeral-pg.cabal` and `CHANGELOG.md`.
- Commit with a Conventional Commits message: `chore(release): ephemeral-pg <new-version>`
- Create an annotated tag: `git tag -a v<new-version> -m "Release <new-version>"`
- Push the commit and tag: `git push && git push --tags`

### 6. Publish to Hackage

Publishing is **permanent and cannot be undone**. Confirm with the user first.

The Justfile has the canonical recipes, but `just publish` and `just release` block on an
interactive `read` confirmation prompt — they will hang a non-interactive agent. Either ask
the user to run `just release` themselves in their terminal, or run the underlying commands
directly:

- Package: `cabal sdist`, then `cabal upload --publish <tarball-path>`
- Docs: `cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump`,
  then `cabal upload --publish --documentation <docs-tarball-path>`

To rehearse without publishing, use `just upload-candidate` and `just upload-docs-candidate`
(these upload Hackage *candidates* and are reversible).

Uploading needs Hackage credentials configured in the user's cabal config; if the upload
fails on authentication, report it rather than retrying.

Report the Hackage URL (`https://hackage.haskell.org/package/ephemeral-pg-<version>`) when done.

## Important

- Always ask the user to confirm the version bump and changelog before committing, and
  confirm again before publishing.
- Never skip `cabal check` or the test suite, and always run them before tagging.
- If any step fails, stop and report the error rather than continuing.
