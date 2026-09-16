---
type: Navigation
title: ephemeral-pg Guides
description: Route readers to the ephemeral-pg migration guide, the suite fixture pattern, and the temporary-root cleanup explanation.
docId: DOC-1
tags: [ephemeral-pg, guides, navigation]
generated:
  by: process:claude-code
  at: 2026-09-16T16:55:00Z
---

# ephemeral-pg Guides

Longer-form documentation that does not belong in the
[README](../../README.md). Start where your question is:

- **Coming from tmp-postgres?**
  [Migrating from tmp-postgres to ephemeral-pg](migrating-from-tmp-postgres.md)
  maps every API you are already using onto its ephemeral-pg equivalent.
- **Large suite, expensive migrations?**
  [Suite-level template databases](suite-template-databases.md) starts one
  cached server per suite and clones a migrated template database per example.
- **Postmasters surviving a killed test run?**
  [Temporary roots and stale cleanup](temporary-roots-and-stale-cleanup.md)
  explains why the sweep is scoped to `temporaryRoot` and why a per-session
  `$TMPDIR` defeats it.

For the ownership and signalling protocol behind the sweep, see
[ADR 1: Stale instance ownership and reaping](../adr/1-stale-instance-ownership-and-reaping.md).
