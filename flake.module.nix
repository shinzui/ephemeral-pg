# flake.module.nix — project-specific flake-parts customizations for ephemeral-pg.
#
# seihou does NOT manage this file: `seihou run` and module migrations never
# regenerate or overwrite it, so everything here survives nix-haskell-flake
# template upgrades without a conflict. (flake.nix and nix/*.nix ARE managed.)
#
# What lives here, relocated out of the formerly hand-written nix/haskell.nix
# when this project adopted the nix-haskell-flake module:
#   - the extra dev-shell runtime packages (postgres client, xz, platform bits)
#   - the `test` dev shell (pinned toolchain, no HLS, no git hooks, no dev DB)
#
# The ephemeral dev-database bring-up (PGHOST/PGDATA/initdb/PG_CONNECTION_STRING)
# now lives in .envrc.local, which direnv sources after the dev-shell shellHook.
{ inputs, ... }:
{
  perSystem = { system, pkgs, ... }:
    let
      hsdev = inputs.haskell-nix-dev.lib.${system};

      # Runtime tools the dev shell needs beyond the base toolchain: the postgres
      # client/server (for the ephemeral dev DB in .envrc.local) and xz, plus
      # platform-specific helpers.
      runtimePackages = [ pkgs.postgresql pkgs.xz ]
        ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.procps pkgs.glibcLocales ]
        ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.lsof ];
    in
    {
      # Adds runtimePackages to the managed devShells.default / .ghc9124 without
      # editing nix/haskell.nix (reads the option declared there).
      haskellProject.extraDevPackages = runtimePackages;

      # Same pinned toolchain as the default shell, but without editor tools, git
      # hooks, or the persistent dev database — for CI / `nix develop .#test`.
      devShells.test = hsdev.mkDevShell {
        ghc = "ghc9124";
        withHls = false;
        extraNativeBuildInputs = runtimePackages;
      };
    };
}
