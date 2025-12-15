#!/usr/bin/env bash
set -euo pipefail

# Default to candidate upload (safer)
UPLOAD_TYPE="--candidate"
UPLOAD_DOCS="false"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --publish)
      UPLOAD_TYPE="--publish"
      shift
      ;;
    --candidate)
      UPLOAD_TYPE="--candidate"
      shift
      ;;
    --with-docs)
      UPLOAD_DOCS="true"
      shift
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --candidate   Upload as candidate (default, safe preview)"
      echo "  --publish     Publish to Hackage (makes package publicly available)"
      echo "  --with-docs   Build and upload documentation (only works with --publish)"
      echo "  --help        Show this help message"
      echo ""
      echo "Examples:"
      echo "  $0                           # Upload as candidate (no docs)"
      echo "  $0 --publish                 # Publish packages without docs"
      echo "  $0 --publish --with-docs     # Publish packages with documentation"
      exit 0
      ;;
    *)
      echo "❌ Unknown option: $1"
      echo "Use --help for usage information"
      exit 1
      ;;
  esac
done

# Validate options
if [ "$UPLOAD_DOCS" = "true" ] && [ "$UPLOAD_TYPE" = "--candidate" ]; then
  echo "⚠️  Warning: Documentation cannot be uploaded for candidates."
  echo "Documentation will only be uploaded when using --publish flag."
  echo ""
  read -p "Continue without uploading documentation? (y/N) " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 1
  fi
  UPLOAD_DOCS="false"
fi

echo "📦 Publish to Hackage Script"
echo "============================"
echo ""

if [ "$UPLOAD_TYPE" = "--publish" ]; then
  echo "⚠️  Mode: PUBLISH (will make packages publicly available)"
else
  echo "ℹ️  Mode: CANDIDATE (safe preview, not publicly available)"
fi
echo ""

# Check if HACKAGE_TOKEN is set
if [ -z "$HACKAGE_TOKEN" ]; then
  echo "❌ Error: HACKAGE_TOKEN environment variable is not set"
  echo "Please set it with: export HACKAGE_TOKEN=your-token-here"
  exit 1
fi

# List of publishable projects
PROJECTS=(
  "eventium-core"
  "eventium-memory"
  "eventium-postgresql"
  "eventium-sql-common"
  "eventium-sqlite"
  "eventium-test-helpers"
)

echo "📝 Step 1: Running hpack on all package.yaml files..."
find . -name "package.yaml" -exec hpack {} \;
echo "✅ hpack completed"
echo ""

echo "📦 Step 2: Creating source distributions for non-example projects..."
for project in "${PROJECTS[@]}"; do
  echo "  Building sdist for $project..."
  cabal sdist "$project"
done
echo "✅ All sdist packages created"
echo ""

echo "📤 Step 3: Uploading packages to Hackage..."
SDIST_DIR="dist-newstyle/sdist"

if [ ! -d "$SDIST_DIR" ]; then
  echo "❌ Error: $SDIST_DIR directory not found"
  exit 1
fi

# Find all .tar.gz files in the sdist directory
TAR_FILES=$(find "$SDIST_DIR" -name "*.tar.gz")

if [ -z "$TAR_FILES" ]; then
  echo "❌ Error: No .tar.gz files found in $SDIST_DIR"
  exit 1
fi

# Upload each tarball
for tarball in $TAR_FILES; do
  echo "  Uploading $(basename "$tarball")..."
  cabal upload "$UPLOAD_TYPE" --token "$HACKAGE_TOKEN" "$tarball"
done

echo ""
if [ "$UPLOAD_TYPE" = "--publish" ]; then
  echo "✅ All packages successfully published to Hackage!"
else
  echo "✅ All packages successfully uploaded as candidates!"
fi
echo ""

# Build and upload documentation if requested
if [ "$UPLOAD_DOCS" = "true" ]; then
  echo "📚 Step 4: Building documentation for Hackage..."
  echo ""
  
  for project in "${PROJECTS[@]}"; do
    echo "  Building documentation for $project..."
    if cabal haddock --haddock-for-hackage "$project"; then
      echo "  ✅ Documentation built for $project"
    else
      echo "  ⚠️  Failed to build documentation for $project"
    fi
  done
  
  echo ""
  echo "✅ Documentation build completed"
  echo ""
  
  echo "📤 Step 5: Uploading documentation to Hackage..."
  DOC_FILES=$(find dist-newstyle -name "*-docs.tar.gz" 2>/dev/null)
  
  if [ -z "$DOC_FILES" ]; then
    echo "⚠️  Warning: No documentation tarballs found in dist-newstyle/"
    echo "Documentation may have failed to build."
  else
    DOC_COUNT=0
    for doc_tarball in $DOC_FILES; do
      echo "  Uploading $(basename "$doc_tarball")..."
      if cabal upload -d --publish --token "$HACKAGE_TOKEN" "$doc_tarball"; then
        ((DOC_COUNT++))
      else
        echo "  ⚠️  Failed to upload $(basename "$doc_tarball")"
      fi
    done
    echo ""
    echo "✅ Uploaded documentation for $DOC_COUNT package(s)"
  fi
  echo ""
fi

echo "📋 Summary:"
echo "  - Ran hpack on all package.yaml files"
echo "  - Created source distributions for ${#PROJECTS[@]} packages"
if [ "$UPLOAD_TYPE" = "--publish" ]; then
  echo "  - Published all packages to Hackage (publicly available)"
  if [ "$UPLOAD_DOCS" = "true" ]; then
    echo "  - Built and uploaded documentation"
  fi
else
  echo "  - Uploaded all packages as candidates (preview mode)"
  echo ""
  echo "💡 To publish for real, run: $0 --publish"
  echo "💡 To publish with docs, run: $0 --publish --with-docs"
fi

