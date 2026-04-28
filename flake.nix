{
  description = "Ocelot: a Game Boy and Game Boy Color emulator in Haskell";

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
            ];

            # Tell Stack to use the GHC from this shell rather than downloading
            # its own (which would not run on NixOS due to the dynamic linker).
            shellHook = ''
              export STACK_NIX_INTEGRATION=1
              export STACK_BUILD_FLAGS="--system-ghc --no-install-ghc"
              alias sb='stack build $STACK_BUILD_FLAGS'
              alias st='stack test  $STACK_BUILD_FLAGS'
              echo "ocelot dev shell: ghc $(ghc --numeric-version), stack $(stack --numeric-version 2>/dev/null || true)"
              echo "Run stack with: stack build --system-ghc --no-install-ghc"
            '';
          };
        });

      formatter = forAllSystems (pkgs: pkgs.nixpkgs-fmt);
    };
}
