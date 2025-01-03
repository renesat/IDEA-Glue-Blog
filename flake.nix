{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    devshell.url = "github:numtide/devshell";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.devshell.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
      perSystem = {
        self',
        config,
        lib,
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
            passthru.optional-dependencies = {
              pygments = [pygments];
            };
            doCheck = false;
          };

        buildInputs = with pkgs; [
          inkscape
          imagemagick
          (python3.withPackages (ps: [
            ps.pygments
            catppuccin
          ]))
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
        blog-dev = blog.overrideAttrs (_: {
          src = self;
        });
      in {
        haskellProjects.default = {
          devShell = {
            hlsCheck.enable = false;
          };
          packages = {
          };
          autoWire = [
            "packages"
            "apps"
            "checks"
          ];
        };

        pre-commit.settings.hooks = {
          alejandra.enable = true;
          deadnix.enable = true;
          statix.enable = true;
          fourmolu.enable = true;
          hlint.enable = true;
          hpack.enable = true;
          denolint.enable = true;
          prettier = {
            enable = true;
            settings = {
              single-quote = false;
            };
            excludes = [
              "^flake.lock$"
            ];
          };
        };

        packages = {
          inherit blog blog-dev;
          default = self'.packages.IDEA-Glue-Blog;
        };
        apps = {
          default = self'.apps.site;
        };

        devshells.default = {
          devshell = {
            packagesFrom = [config.haskellProjects.default.outputs.devShell];
            startup.pre-commit.text = config.pre-commit.installationScript;
          };
          packages = buildInputs;
          commands = [
            {
              package = pkgs.nodePackages.prettier;
              help = "Opinionated multi-language code formatter";
            }
            {
              package = pkgs.just;
              help = "Just a command runner";
            }
            {
              package = pkgs.alejandra;
              help = "Format nix code";
            }
            {
              package = pkgs.haskellPackages.fourmolu;
              help = "A formatter for Haskell source code";
            }
            {
              package = pkgs.nix-output-monitor;
              help = "Nix Output Monitor (a drop-in alternative for `nix` which shows a build graph)";
            }
          ];
          name = "IDEA-Glue-Blog";
        };
      };
    };
}
