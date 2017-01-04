let

pkgsOrig = import (fetchTarball https://nixos.org/channels/nixos-16.09/nixexprs.tar.xz) {};

pkgs = pkgsOrig.overridePackages (self: super: {
  haskellPackages = super.haskell.packages.ghc7102.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        doHaddock = false;
        doCheck = false;
        doHoogle = false;
      });
    };
  };
});

ghc = pkgs.haskellPackages.ghcWithPackages
  (pkgs: with pkgs; [ hakyll filepath parsec ]);

in pkgs.stdenv.mkDerivation {
  name = "atnnn.com";
  buildInputs = [ ghc ];
  shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
