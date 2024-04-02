{
  description = "A Lua programming game written in OCaml";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    ocaml-lua.url = "github:kenranunderscore/ocaml-lua";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, pkgs, system, self', ... }:
        let ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
        in {

          packages.default = ocamlPackages.buildDunePackage {
            pname = "ocamluarena";
            version = "git";
            src = ./.;
            buildInputs = [
              inputs.ocaml-lua.packages.${system}.default
              ocamlPackages.tsdl
              ocamlPackages.tsdl-image
              ocamlPackages.ppxlib
              ocamlPackages.ppx_deriving
              ocamlPackages.alcotest
              pkgs.SDL2
              pkgs.SDL2_image
              pkgs.libffi
              pkgs.lua5_1
              pkgs.pkg-config
            ];
            doCheck = true;

            meta.mainProgram = "arena";
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [ self'.packages.default ];
            packages = [
              ocamlPackages.ocaml
              ocamlPackages.dune_3
              ocamlPackages.findlib
              ocamlPackages.ocaml-lsp
              ocamlPackages.merlin
              ocamlPackages.ocamlformat
              ocamlPackages.utop
            ];
          };
        };
    };
}
