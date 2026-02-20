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
