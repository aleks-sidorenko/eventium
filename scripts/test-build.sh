#!/bin/bash

echo "🔧 Test and Build Script for Eventful"
echo "====================================="
echo ""

echo "🎯 Entering Nix development environment..."
nix develop --command bash << 'EOF'
  echo "📦 GHC version: $(ghc --version)"
  echo "🔧 Cabal version: $(cabal --version)"
  echo ""
  
  echo "📝 Regenerating cabal files from package.yaml..."
  find . -name "package.yaml" -exec hpack {} \;
  echo ""
  
  echo "🔨 Building all packages..."
  cabal build all
  echo ""
  
  echo "🧪 Running tests..."
  echo "Testing core packages:"
  cabal test eventium-core eventium-memory eventium-testkit
  echo ""
  
  echo "Testing examples:"  
  cabal test examples-bank
  echo ""
  
  echo "✅ Build and test complete!"
EOF

echo "📋 Summary of fixes applied:"
echo "- ✅ Added hspec-discover to build-tools in test configurations"
echo "- ✅ Added hspec-discover to Nix environment (flake.nix and shell.nix)"
echo "- ✅ Fixed duplicate maintainer fields in package.yaml files"
echo "- ✅ Replaced sum-type-boilerplate with x-sum-type-boilerplate (GHC 9.6 compatible)"
echo "- ✅ Restored Template Haskell functions with modern package"
echo "- ✅ Updated SQL backends to use modern persistent API (FieldNameDB/EntityNameDB)"
echo "- ✅ Fixed Template Haskell API compatibility for GHC 9.6"
echo "- ✅ Added SafeToInsert constraints for modern persistent insertion safety" 