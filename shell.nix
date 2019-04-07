let

pkgs = import (fetchTarball https://nixos.org/channels/nixos-18.09/nixexprs.tar.xz) {};

ghc = pkgs.ghc.withPackages
  (pkgs: with pkgs; [
    hakyll
  ]);

in pkgs.stdenv.mkDerivation {
  name = "atnnn.com";
  buildInputs = [ ghc pkgs.cabal-install pkgs.git ];
}
