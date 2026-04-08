# Eventium - Development Commands
# Usage: just <recipe>
# List all recipes: just --list

# Default recipe to display help
default:
    @just --list

# Generate cabal files from package.yaml
hpack:
    @echo "Generating .cabal files from package.yaml..."
    find . -name package.yaml -exec dirname {} \; | while read dir; do \
        echo "  → $dir"; \
        (cd "$dir" && hpack); \
    done
    @echo "✓ Done"

# Build all packages
build:
    @echo "Building all packages..."
    cabal build all
    @echo "✓ Build complete"

# Run all tests
test:
    @echo "Running tests..."
    cabal test all
    @echo "✓ Tests complete"

# Run hlint
lint:
    @echo "Linting code..."
    hlint --git
    @echo "✓ Lint complete"

# Format all Haskell source files with ormolu
format:
    @echo "Formatting code..."
    find . -name '*.hs' -not -path './dist-newstyle/*' -exec ormolu --mode inplace {} +
    @echo "✓ Format complete"

# Format and lint code
check: format lint
    @echo "✓ Code quality checks complete"

# Continuous compilation
ghcid *ARGS:
    @echo "Starting continuous compilation..."
    ghcid {{ARGS}}

# Start PostgreSQL with Docker Compose
db-up:
    @echo "Starting PostgreSQL..."
    docker compose up -d postgres
    @echo "✓ PostgreSQL started"
    @echo "Waiting for PostgreSQL to be ready..."
    @sleep 3
    -docker compose exec -T postgres pg_isready -U postgres || echo "PostgreSQL not ready yet..."

# Stop PostgreSQL
db-down:
    @echo "Stopping PostgreSQL..."
    docker compose down
    @echo "✓ PostgreSQL stopped"

# Stop PostgreSQL and remove volumes
db-reset:
    @echo "Stopping PostgreSQL and removing volumes..."
    docker compose down -v
    @echo "✓ PostgreSQL stopped and volumes removed"

# Show PostgreSQL logs
db-logs:
    docker compose logs -f postgres

# Connect to PostgreSQL with psql
db-psql:
    docker compose exec postgres psql -U postgres -d eventium_test

# Restart PostgreSQL
db-restart: db-down db-up
    @echo "✓ PostgreSQL restarted"

# Publish to Hackage (candidate by default, use --publish to publish for real)
hackage-publish *ARGS:
    ./scripts/publish-to-hackage.sh {{ARGS}}
