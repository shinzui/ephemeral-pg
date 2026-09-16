---
okf_version: "0.2"
---

# Explanation

- [Temporary roots and stale cleanup](temporary-roots-and-stale-cleanup.md) - Why stale-instance sweeping is scoped to a single temporary root, and how an unstable $TMPDIR silently leaks abandoned clusters.

# Guide

- [Migrating from tmp-postgres to ephemeral-pg](migrating-from-tmp-postgres.md) - Port a test suite from tmp-postgres to ephemeral-pg across connections, configuration, caching, snapshots, and error handling.
- [Suite-level template databases](suite-template-databases.md) - Start one cached PostgreSQL server per suite and clone a migrated template database per example for fast, isolated fixtures.

# Navigation

- [ephemeral-pg Guides](README.md) - Route readers to the ephemeral-pg migration guide, the suite fixture pattern, and the temporary-root cleanup explanation.

