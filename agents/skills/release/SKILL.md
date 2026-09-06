---
name: release
description: Release a new version of the Haskell package following PVP
argument-hint: "[major|minor|patch]"
disable-model-invocation: true
allowed-tools: Read, Bash, Edit, Glob, Grep, Write
---

# Release Skill

Release a new version of the ephemeral-pg package to Hackage.

## Arguments

`$ARGUMENTS` is an optional version bump hint: `major`, `minor`, or `patch`.
If omitted, determine the bump level from the changes (see step 2).

## Steps

### 1. Determine what changed since the last release

- Read the current version from `ephemeral-pg.cabal` (the `version:` field).
- Find the latest git tag matching `v*` to identify the last release point.
- Run `git log --oneline <last-tag>..HEAD` to list commits since the last release.
- If there are no commits since the last tag, inform the user there is nothing to release and stop.

### 2. Determine the next version using PVP

The Haskell PVP version format is `A.B.C.D`:
- `A.B` is the **major** version — bump for breaking API changes (removed/renamed exports, changed types, changed semantics)
- `C` is the **minor** version — bump for backwards-compatible API additions (new exports, new modules, new type class instances)
- `D` is the **patch** version — bump for bug fixes, documentation, internal-only changes, performance improvements

Rules:
- If `$ARGUMENTS` is `major`, `minor`, or `patch`, use that bump level.
- Otherwise, analyze the commits to determine the appropriate bump:
  - Look for keywords like "breaking", "remove", "rename", "change type" → major
  - Look for keywords like "add", "new", "feature", "export" → minor
  - Look for keywords like "fix", "suppress", "docs", "refactor", "internal" → patch
- Present the proposed bump to the user and ask for confirmation before proceeding.

Increment the version:
- **major**: increment `B`, reset `C` and `D` to 0 (e.g. `0.2.0.1` → `0.3.0.0`)
- **minor**: increment `C`, reset `D` to 0 (e.g. `0.2.0.1` → `0.2.1.0`)
- **patch**: increment `D` (e.g. `0.2.0.1` → `0.2.0.2`)

### 3. Update version and changelog

- Edit `ephemeral-pg.cabal` to set the new version.
- Edit `CHANGELOG.md` to add a new section for the new version above the previous version. Summarize the changes from the commit log, grouped by category:
  - **Breaking Changes** (if major)
  - **New Features** (if minor or major)
  - **Bug Fixes** (if any)
  - **Other Changes** (docs, refactoring, etc.)
  - Only include categories that have entries.
- Show the user the changelog entry and version bump for review before committing.

### 4. Commit, tag, and push

- Stage `ephemeral-pg.cabal` and `CHANGELOG.md`.
- Commit with message: `Bump version to <new-version> for release`
- Create an annotated git tag: `git tag -a v<new-version> -m "Release <new-version>"`
- Push the commit and tag: `git push && git push --tags`

### 5. Publish to Hackage

- Run `cabal check` to verify no packaging issues.
- Run `cabal test` to ensure tests pass.
- Run `cabal sdist` and then `cabal upload --publish <tarball-path>` to publish.
- Run `cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump` and then `cabal upload --publish --documentation <docs-tarball-path>` to publish docs.
- Report the Hackage URL to the user when done.

## Important

- Always ask the user to confirm the version bump and changelog before committing.
- Never skip tests or `cabal check`.
- If any step fails, stop and report the error rather than continuing.
