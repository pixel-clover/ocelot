{
  description = "Ocelot: a Gameboy and Gameboy Color emulator in Haskell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems
          (system: f (import nixpkgs { inherit system; }));
    in
    {
      devShells = forAllSystems (pkgs:
        let
          ghc = pkgs.haskell.compiler.ghc966;
          hsPkgs = pkgs.haskell.packages.ghc966;
        in
        {
          default = pkgs.mkShell {
            name = "ocelot";

            packages = [
              ghc
              pkgs.stack
              pkgs.cabal-install
              hsPkgs.haskell-language-server
              hsPkgs.hlint
              hsPkgs.fourmolu
              pkgs.hpack
              pkgs.zlib
              pkgs.pkg-config
              # SDL2 for the frontend (which is linked at runtime via sdl2 Haskell package).
              pkgs.SDL2
            ];

            # Stack picks up this GHC via system-ghc/install-ghc settings in stack.yaml, so no extra flags are needed at the command line.
            shellHook = ''
              echo "Ocelot dev shell: ghc $(ghc --numeric-version), stack $(stack --numeric-version 2>/dev/null || true)"
            '';
          };
        });

      formatter = forAllSystems (pkgs: pkgs.nixpkgs-fmt);
    };
}
