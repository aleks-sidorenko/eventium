# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Eventium is a Haskell event sourcing library (forked and modernized from `eventful`). It provides abstract event store interfaces, pure projections, command handlers, process managers, and pluggable storage backends (in-memory, PostgreSQL, SQLite).

## Build & Development

**Prerequisites:** Nix (with flakes) + direnv. Entering the directory auto-loads the dev environment via `.envrc` → `nix develop`. GHC 9.6.7 is pinned.

**Cabal files are generated from `package.yaml` via hpack.** After changing any `package.yaml`, run `just hpack` before building.

```
just build          # cabal build all
just test           # cabal test all
just hpack          # regenerate .cabal files from package.yaml
just format         # ormolu --mode inplace on all .hs files
just ghcid          # continuous compilation
```

**Running a single package's tests:**
```
cabal test eventium-core
cabal test eventium-memory
cabal test eventium-postgresql
cabal test eventium-sqlite
cabal test examples-bank
```

**PostgreSQL tests** require a running Postgres instance. Use `docker compose up -d` to start one. Tests use these env vars (with defaults matching docker-compose.yaml):
- `POSTGRES_HOST` (127.0.0.1), `POSTGRES_PORT` (5432), `POSTGRES_USER` (postgres), `POSTGRES_PASSWORD` (password), `POSTGRES_DBNAME` (eventium_test)

**Linting:** `hlint` (run on CI against all git-tracked `.hs` files). **Formatting:** `ormolu`.

## Package Architecture

Multi-package cabal project (`cabal.project`):

- **eventium-core** — Abstract interfaces and pure types. Key abstractions:
  - `EventStoreReader`/`EventStoreWriter` — polymorphic over key, position, monad, and event types (uses RankNTypes)
  - `Projection` — pure state machine: seed state + event handler fold
  - `CommandHandler` — combines a Projection with validation to produce events
  - `ProcessManager`, `EventSubscription`, `EventPublisher` — orchestration primitives
  - `Codec` — bidirectional event encoding/decoding (encode/decode)
  - `TypeEmbedding` — sum-type subset relationships (embed/extract)
  - `Eventium.TH` — Template Haskell codegen for projections, sum-type codecs, and embeddings (uses `x-sum-type-boilerplate`)
- **eventium-memory** — STM-based in-memory `EventStoreReader`/`Writer` and `ProjectionCache`. Primary backend for tests.
- **eventium-sql-common** — Shared persistent entity definitions and SQL operations for SQL backends.
- **eventium-postgresql** — PostgreSQL backend via `persistent`/`persistent-postgresql`.
- **eventium-sqlite** — SQLite backend via `persistent`/`persistent-sqlite`.
- **eventium-testkit** — Shared hspec test utilities used across all packages.
- **examples/{bank,cafe,counter-cli}** — Example applications demonstrating aggregates, command handlers, process managers, and read models.

## Testing Conventions

- Tests use **hspec** with **hspec-discover** for automatic test module discovery.
- Each package has a `tests/Spec.hs` that serves as the discovery entry point.
- Integration tests for SQL backends require their respective databases running.
- The `eventium-memory` package contains the most comprehensive behavioral tests (command handlers, event publishers, subscriptions, process managers) since it tests core abstractions via the in-memory backend.

## CI

GitHub Actions (`.github/workflows/ci.yml`): builds all packages, runs hlint, runs all tests with a PostgreSQL 15 service container. Triggers on pushes to main/master, tags, and PRs.
