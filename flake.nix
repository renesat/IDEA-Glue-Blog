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
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
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
        inputs.treefmt-nix.flakeModule
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

        treefmt = {
          programs = {
            fourmolu.enable = true;
            alejandra.enable = true;
            taplo.enable = true;
            prettier = {
              enable = true;
            };
            yamlfmt = {
              enable = true;
            };
          };
        };

        pre-commit.settings.hooks = {
          treefmt = {
            enable = true;
            package = config.treefmt.build.wrapper;
          };
          deadnix = {
            enable = true;
            args = ["--edit"];
          };
          zizmor = {
            enable = true;
            name = "zizmor";
            description = "Find security issues in GitHub Actions CI/CD setups";
            types = ["yaml"];
            files = "(\.github/workflows/.*)|(action\.ya?ml)$";
            require_serial = true;
            entry = lib.getExe pkgs.zizmor;
          };
          statix.enable = true;
          nil.enable = true;
          ripsecrets.enable = true;
          hlint.enable = true;
          hpack.enable = true;
          denolint.enable = true;
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
              package = pkgs.just;
              help = "Just a command runner";
            }
            {
              package = config.treefmt.build.wrapper;
              help = "Format all";
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
