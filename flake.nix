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
          pkgs.haskellPackages.callCabal2nix "IDEA-Glue-Blog-Builder" self {};

        buildInputs = with pkgs; [
            inkscape
            imagemagick
            python3Packages.pygments
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
            buildInputs = with pkgs; [
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
