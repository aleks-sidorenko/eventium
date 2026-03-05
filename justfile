# Build all packages
build:
    cabal build all

# Run all tests
test:
    cabal test all

# Generate cabal files from package.yaml
hpack:
    find . -name package.yaml -exec dirname {} \; | while read dir; do \
        echo "  → $dir"; \
        (cd "$dir" && hpack); \
    done

# Run hlint
lint:
  hlint --git

# Continuous compilation
ghcid *ARGS:
    ghcid {{ARGS}}

# Format all Haskell source files with ormolu
format:
    find . -name '*.hs' -not -path './dist-newstyle/*' -exec ormolu --mode inplace {} +

# Start PostgreSQL via Docker Compose
docker-up:
    docker compose up -d postgres
    @echo "Waiting for PostgreSQL to be ready..."
    @sleep 3
    -docker compose exec -T postgres pg_isready -U postgres || echo "PostgreSQL not ready yet..."

# Stop PostgreSQL
docker-down:
    docker compose down

# Stop PostgreSQL and remove volumes
docker-down-volumes:
    docker compose down -v

# Show PostgreSQL logs
docker-logs:
    docker compose logs -f postgres

# Connect to PostgreSQL with psql
docker-psql:
    docker compose exec postgres psql -U postgres -d eventium_test

# Restart PostgreSQL
docker-restart: docker-down docker-up

# Publish to Hackage (candidate by default, use --publish to publish for real)
hackage-publish *ARGS:
    ./scripts/publish-to-hackage.sh {{ARGS}}
