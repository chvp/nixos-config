#!/usr/bin/env bash
set -euo pipefail
set -x

git pull

if [ -z "${OVERRIDE:-}" ]
then
    su -c "nixos-rebuild --flake . switch"
else
    su -c "nixos-rebuild --flake . --override-input nixpkgs ../nixpkgs --no-write-lock-file switch"
fi
