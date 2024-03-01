{
  description = "A very basic flake";

  inputs = { flake-parts.url = "github:hercules-ci/flake-parts"; };

  outputs = inputs@{ self, nixpkgs, flake-parts }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, pkgs, ... }: {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ SDL2 SDL2_image libffi lua5_1 pkg-config ];
        };
      };
    };
}
