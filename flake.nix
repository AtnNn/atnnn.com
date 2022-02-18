{
  inputs = {
    nixpkgs.url = "https://nixos.org/channels/nixos-21.11/nixexprs.tar.xz";
  };
  outputs = { self, nixpkgs }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
  in {
    devShell."x86_64-linux" = pkgs.stdenv.mkDerivation {
      name = "atnnn.com";
      buildInputs = [
        (pkgs.ghc.withPackages (pkgs: with pkgs; [ hakyll ]))
        pkgs.cabal-install
        pkgs.git
      ];
    };
  };
}
