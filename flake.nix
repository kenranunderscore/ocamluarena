{
  description = "A Lua programming game written in OCaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    ocaml-lua = {
      url = "github:kenranunderscore/ocaml-lua";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ocaml-lua }:
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
              ocaml-lua.packages.${system}.default
              ocamlPackages.alcotest
              ocamlPackages.ppx_deriving
              ocamlPackages.ppxlib
              ocamlPackages.tsdl
              ocamlPackages.tsdl-image
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
              ocamlPackages.dune_3
              ocamlPackages.findlib
              ocamlPackages.merlin
              ocamlPackages.ocaml
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat
              ocamlPackages.utop
            ];
          };
        };
    };
}
