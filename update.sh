#!/usr/bin/env bash
set -euo pipefail
set -x

nix flake update

if [ -z "${OVERRIDE:-}" ]
then
    su -c "nixos-rebuild --flake . --build-host root@urithiru switch"
else
    su -c "nixos-rebuild --flake . --build-host root@urithiru --override-input nixpkgs ../nixpkgs --no-write-lock-file switch"
fi
