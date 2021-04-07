#!/usr/bin/env bash
set -euo pipefail
set -x

nix flake update

if [ -z "${OVERRIDE:-}" ]
then
    nix build --no-link --profile /nix/var/nix/profiles/per-user/charlotte/`hostname` .#nixosConfigurations.`hostname`.config.system.build.toplevel
    su -c "nixos-rebuild --flake . switch"
else
    nix build --no-link --override-input nixpkgs ../nixpkgs --no-write-lock-file --profile /nix/var/nix/profiles/per-user/charlotte/`hostname` .#nixosConfigurations.`hostname`.config.system.build.toplevel
    su -c "nixos-rebuild --flake . --override-input nixpkgs ../nixpkgs --no-write-lock-file switch"
fi
