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
build: hpack
    @echo "Building all packages..."
    cabal build all
    @echo "✓ Build complete"

# Run all tests
test:
    @echo "Running tests..."
    cabal test all --test-show-details=direct --enable-tests
    @echo "✓ Tests complete"

# Run tests with coverage
test-coverage:
    @echo "Running tests with coverage..."
    cabal test all --enable-coverage --test-show-details=direct --enable-tests

# Clean build artifacts
clean:
    @echo "Cleaning build artifacts..."
    cabal clean
    rm -rf dist-newstyle/
    @echo "✓ Clean complete"

# Format all Haskell source files with ormolu
format:
    @echo "Formatting code..."
    find . -name '*.hs' -not -path './dist-newstyle/*' -exec ormolu --mode inplace {} +
    @echo "✓ Format complete"

# Check formatting without modifying files
format-check:
    @echo "Checking code formatting..."
    find . -name '*.hs' -not -path './dist-newstyle/*' -exec ormolu --mode check {} +
    @echo "✓ Format check complete"

# Lint code with hlint (read-only)
lint:
    @echo "Linting code..."
    hlint --git
    @echo "✓ Lint complete"

# Auto-fix lint issues with hlint --refactor (requires apply-refact)
lint-fix:
    @echo "Auto-fixing lint issues..."
    hlint --git --refactor --refactor-options="-i"
    @echo "✓ Lint fix complete"

# Read-only quality umbrella: format check + lint (CI-safe)
check: format-check lint
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

# Clean and rebuild
rebuild: clean build

# Run all checks and build
all: check test build
    @echo "✓ All tasks complete"

# Update dependencies
update:
    @echo "Updating dependencies..."
    cabal update
    @echo "✓ Dependencies updated"

# Generate REPL session
repl:
    cabal repl

# Generate documentation
docs:
    @echo "Generating documentation..."
    cabal haddock all --haddock-hyperlink-source
    @echo "✓ Documentation generated in dist-newstyle/"

# Show project info
info:
    @echo "Project: Eventium"
    @echo "GHC: $(ghc --version)"
    @echo "Cabal: $(cabal --version | head -n1)"
    @echo "Database: PostgreSQL 15 (Docker Compose)"
    @echo ""
    @echo "Quick commands:"
    @echo "  just build       - Build all packages"
    @echo "  just test        - Run tests"
    @echo "  just db-up       - Start PostgreSQL"
    @echo "  just check       - Format check and lint"

# --- CI ---

# Trigger CI workflow for the current branch
ci:
    gh workflow run CI --ref "$(git branch --show-current)"

# --- Hackage ---

# Publish to Hackage (candidate by default, use --publish to publish for real)
publish *ARGS:
    ./scripts/publish.sh {{ARGS}}
