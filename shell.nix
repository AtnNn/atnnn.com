with (import <nixpkgs> {});

let ghc = haskell.packages.ghc7102.ghcWithPackages
  (pkgs: with pkgs; [ hakyll filepath parsec ]); in

stdenv.mkDerivation {
  name = "atnnn.com";
  buildInputs = [ ghc ];
  shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
