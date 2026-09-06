# Dev shell, built from the haskell-nix-dev base flake's mkDevShell (GHC 9.12.4 +
# cabal + HLS). This is a dev-shell-only flake: there is no package build.
#
# mkDevShell already provides: the GHC compiler, cabal, HLS (when withHls),
# pkg-config, and zlib, plus a LANG=en_US.UTF-8 export. Only list tools BEYOND
# those in extraNativeBuildInputs (e.g. pkgs.just, pkgs.postgresql, pkgs.xz).
{ inputs, lib, flake-parts-lib, ... }:
{
  options.perSystem = flake-parts-lib.mkPerSystemOption ({ ... }: {
    options.haskellProject.extraDevPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      example = lib.literalExpression "[ pkgs.ghciwatch ]";
      description = "Extra packages to add to the dev shell.";
    };
  });

  config.perSystem = { system, pkgs, config, ... }:
    let
      hsdev = inputs.haskell-nix-dev.lib.${system};
      runtimePackages = [ pkgs.postgresql pkgs.xz ]
        ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.procps pkgs.glibcLocales ]
        ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.lsof ];

      mkProjectShell = ghc: hsdev.mkDevShell {
        inherit ghc;
        withHls = true;
        extraNativeBuildInputs =
          runtimePackages ++ [ pkgs.just ]
          ++ config.haskellProject.extraDevPackages;
        shellHook = ''
          ${config.pre-commit.installationScript}

          # Database paths - all relative to project root
          export PGHOST="$PWD/db"
          export PGDATA="$PGHOST/db"
          export PGLOG=$PGHOST/postgres.log
          export PGDATABASE=rei

          # Connection string for application use
          export PG_CONNECTION_STRING=postgresql://$(jq -rn --arg x $PGHOST '$x|@uri')/$PGDATABASE

          # Create directories
          mkdir -p $PGHOST
          mkdir -p .dev

          # Initialize database cluster on first entry
          if [ ! -d $PGDATA ]; then
            initdb --auth=trust --no-locale --encoding=UTF8
          fi
        '';
      };
    in
    {
      devShells.default = mkProjectShell "ghc9124";
      devShells.ghc9124 = mkProjectShell "ghc9124";
      # Same pinned toolchain, without editor tools, Git hooks or a persistent DB.
      devShells.test = hsdev.mkDevShell {
        ghc = "ghc9124";
        withHls = false;
        extraNativeBuildInputs = runtimePackages;
      };
    };
}
