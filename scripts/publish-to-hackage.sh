#!/usr/bin/env bash
set -euo pipefail



echo "📦 Publish to Hackage Script"
echo "============================"
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
  cabal upload --token "$HACKAGE_TOKEN" "$tarball"
done

echo ""
echo "✅ All packages successfully uploaded to Hackage!"
echo ""
echo "📋 Summary:"
echo "  - Ran hpack on all package.yaml files"
echo "  - Created source distributions for ${#PROJECTS[@]} packages"
echo "  - Uploaded all packages to Hackage"

