{
  description = "el-be-back: Emacs terminal emulator built on wezterm-term";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          ext = if pkgs.stdenv.isDarwin then "dylib" else "so";
        in
        {
          default = pkgs.rustPlatform.buildRustPackage {
            pname = "ebb-module";
            version = "0.1.0";
            src = ./.;
            cargoLock = {
              lockFile = ./Cargo.lock;
              outputHashes = {
                "wezterm-term-0.1.0" = "sha256-usmXju7tDaJZRicONAX0oduQPkOeahSJPCExuRt6dt4=";
                "finl_unicode-1.3.0" = "sha256-38S6XH4hldbkb6NP+s7lXa/NR49PI0w3KYqd+jPHND0=";
              };
            };
            # wezterm-term uses include_bytes! with a monorepo-relative path
            # (../../../termwiz/data/wezterm) that breaks under cargo vendoring.
            # Create a symlink so the path resolves correctly.
            preBuild = ''
              ln -sfn "$NIX_BUILD_TOP/cargo-vendor-dir/termwiz-0.24.0" \
                      "$NIX_BUILD_TOP/cargo-vendor-dir/termwiz"
            '';
            # cdylib has no binary to install; copy the .so/.dylib manually.
            # nix builds to target/<triple>/release/, not target/release/.
            installPhase = ''
              runHook preInstall
              mkdir -p $out/lib
              install -m444 target/*/release/libebb_module.${ext} $out/lib/ebb-module.${ext}
              mkdir -p $out/share/emacs/site-lisp
              install -m444 el-be-back.el $out/share/emacs/site-lisp/
              runHook postInstall
            '';
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay.overlays.default ];
          };
          rust = pkgs.rust-bin.stable.latest.default.override {
            extensions = [
              "rust-src"
              "rust-analyzer"
            ];
          };
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              rust
              pkgs.pkg-config
            ];

            RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
          };
        }
      );
    };
}
