{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }: 
    flake-utils.lib.eachDefaultSystem (system: 

      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.nb-lang.flake {};
        overlays = [
          haskellNix.overlay (final: prev: {
            nb-lang = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc962";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
              };
            };
          })];
           
      in
        flake // rec {
          packages.default = flake.packages."nb-lang:exe:nb-lang";
          devShell = pkgs.haskellPackages.shellFor {
            buildInputs = with pkgs.haskellPackages; [ cabal-install haskell-language-server];
          };
        }

    );
}
