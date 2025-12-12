# Simple shell.nix for development
{
  pkgs ? import <nixpkgs> { },
}:

let
  hPkgs = pkgs.haskell.packages.ghc967;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    # Haskell tools
    hPkgs.ghc
    hPkgs.cabal-install
    hPkgs.hpack

    # Database services
    postgresql
    sqlite

    # Development tools
    hPkgs.haskell-language-server
    hPkgs.hlint
    hPkgs.ormolu
    hPkgs.ghcid
    hPkgs.hspec-discover

    # System dependencies
    pkg-config
    zlib
  ];

  shellHook = ''
    echo "🎯 Eventium Development Environment (shell.nix)"
    echo "📦 GHC version: $(ghc --version)"
    echo "Use 'nix develop' for the full flake experience"
  '';
}

