{
  description = "rust";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "github:edolstra/flake-compat";
    naersk.url = "github:nix-community/naersk/master";
  };

  outputs =
    { nixpkgs
    , flake-utils
    , naersk
    , ...
    }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = (import nixpkgs) {
        inherit system;
      };
      naersk-lib = pkgs.callPackage naersk { };
    in
    {
      name = "rust";

      devShells.default = with pkgs; pkgs.mkShell {
        nativeBuildInputs = [
          pkg-config
        ];
        buildInputs = [
          cargo
          rustc
          rustfmt
          rustPackages.clippy
          cargo-watch
          nixpkgs-fmt
          just
        ] ++ lib.optionals stdenv.isDarwin [
          darwin.apple_sdk.frameworks.Security
        ];
      };

      packages.default = naersk-lib.buildPackage ./.;
    });
}
