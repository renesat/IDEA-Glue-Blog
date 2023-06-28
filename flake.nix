{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      systems = [
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem = {
        self',
        system,
        lib,
        config,
        pkgs,
        ...
      }: let
        catppuccin = with pkgs.python3.pkgs;
          buildPythonPackage rec {
            pname = "catppuccin";
            version = "1.2.0";
            src = pkgs.python3.pkgs.fetchPypi {
              inherit pname version;
              sha256 = "sha256-hUNt6RHntQzamDX1SdhBzSj3pR/xxb6lpwzvYnqwOIo=";
            };
            passthru.optional-dependencies = {pygments = [pygments];};
            doCheck = false;
          };

        buildInputs = with pkgs; [
          inkscape
          imagemagick
          (python3.withPackages (ps: [ps.pygments catppuccin]))
          nodePackages.katex
        ];

        blog = pkgs.stdenvNoCC.mkDerivation {
          name = "IDEA-Glue-Blog_static";
          src = pkgs.lib.cleanSource self;
          nativeBuildInputs = buildInputs;
          buildPhase = ''
            ${self'.apps.site.program} build
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/site $out/public
          '';
          LANG = "C.UTF-8";
        };
        blog-dev = blog.overrideAttrs (old: rec {src = self;});
      in {
        haskellProjects.default = {
          devShell = {
            # TODO: Remove this after https://github.com/numtide/treefmt-nix/issues/65
            tools = hp:
              {
                hpack = hp.hpack;
                treefmt = config.treefmt.build.wrapper;
              }
              // config.treefmt.build.programs;
            hlsCheck.enable = false;
          };
          packages = {
          };
          autoWire = ["packages" "apps" "checks"];
        };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.ormolu.package = pkgs.haskellPackages.fourmolu_0_12_0_0.overrideScope (lself: lsuper: {
            Cabal-syntax = lself.Cabal-syntax_3_10_1_0;
            ghc-lib-parser = lself.ghc-lib-parser_9_6_2_20230523;
            parsec = lself.parsec_3_1_16_1;
            text = lself.text_2_0_2;
          });
          programs.alejandra.enable = true;
          programs.hlint.enable = true;
        };

        mission-control.scripts = {
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
          build = {
            description = "Build all";
            exec = ''
              cabal build
            '';
            category = "Dev Tools";
          };
        };

        packages = {
          inherit blog blog-dev;
          default = self'.packages.IDEA-Glue-Blog;
        };
        apps = {
          default = self'.apps.site;
        };

        devShells.default = pkgs.mkShell {
          name = "IDEA-Glue-Blog";
          inputsFrom =
            [
              config.haskellProjects.default.outputs.devShell
              config.flake-root.devShell
              config.mission-control.devShell
            ]
            ++ buildInputs;
        };
      };
    };
}
