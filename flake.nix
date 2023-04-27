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

        blog-builder =
          pkgs.haskellPackages.callCabal2nix "IDEA-Glue-Blog-Builder" self { };

        blog = pkgs.stdenvNoCC.mkDerivation {
          name = "IDEA-Glue-Blog";
          src = pkgs.lib.cleanSource self;
          nativeBuildInputs = with pkgs; [
            inkscape
            imagemagick
            python3Packages.pygments
            nodePackages.katex
          ];
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
        packages = { inherit blog blog-builder blog-dev; };
        packages.default = blog-builder;
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              stack-wrapped
              pkgs.haskellPackages.hasktags
              pkgs.haskellPackages.fourmolu
              pkgs.haskell-language-server
              pkgs.nixfmt
            ];
            NIX_PATH = "nixpkgs=" + pkgs.path;
          };
        };
      });
}
