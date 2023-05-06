{

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [
      "aarch64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          sandbox = false;
        };
        catppuccin = with pkgs.python3.pkgs;
          buildPythonPackage rec {
            pname = "catppuccin";
            version = "1.2.0";
            src = pkgs.python3.pkgs.fetchPypi {
              inherit pname version;
              sha256 = "sha256-hUNt6RHntQzamDX1SdhBzSj3pR/xxb6lpwzvYnqwOIo=";
            };
            passthru.optional-dependencies = { pygments = [ pygments ]; };
            doCheck = false;
          };

        blog-builder =
          pkgs.haskellPackages.callCabal2nixWithOptions "IDEA-Glue-Blog-Builder"
          (pkgs.lib.cleanSourceWith {
            filter = (path: type:
              let
                relPath =
                  pkgs.lib.removePrefix (toString self + "/") (toString path);
              in !(pkgs.lib.hasPrefix "site/" relPath));
            src = self;
          }) "--hpack"{ };

        buildInputs = with pkgs; [
          inkscape
          imagemagick
          (python3.withPackages (ps: [ ps.pygments catppuccin ]))
          nodePackages.katex
        ];
        blog = pkgs.stdenvNoCC.mkDerivation {
          name = "IDEA-Glue-Blog";
          src = pkgs.lib.cleanSource self;
          nativeBuildInputs = buildInputs;
          buildPhase = ''
            ${blog-builder}/bin/site build
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/site $out/public
          '';
          LANG = "C.UTF-8";
        };

        blog-dev = blog.overrideAttrs (old: rec { src = self; });

        stack-nix-integration = pkgs.writeText "stack-nix-integration.nix" ''
          {ghc}:
          let
            pkgs = import ${pkgs.path} {};
          in pkgs.haskell.lib.buildStackProject {
            inherit ghc;
            name = "IDEAGlueBlog";
            buildInputs = with pkgs; [
              libffi
              gmp
              zlib

              inkscape
              nodePackages.katex
              imagemagick
              libaom
              python3Packages.pygments

            ];
          }
        '';

        # Source: https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --nix \
                --no-nix-pure \
                --nix-shell-file="${stack-nix-integration}"\
              "
          '';
        };

      in {
        packages = {
          inherit blog blog-builder blog-dev;
          default = blog-builder;
        };
        apps = {
          builder = {
            type = "app";
            program = "${blog-builder}/bin/site";
          };
          defalut = self.apps."${system}".builder;
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs;
              [
                stack-wrapped

                haskellPackages.hasktags
                haskellPackages.fourmolu
                haskell-language-server
                nixfmt
                html-tidy
              ] ++ buildInputs;
            NIX_PATH = "nixpkgs=" + pkgs.path;
          };
        };
      });
}
