#!/usr/bin/env bash
set -euo pipefail
set -x

if [ -z "${NO_LOCAL:-}" ]
then
    pushd ../nixpkgs
    git fetch --all --prune
    git rebase upstream/nixos-unstable-small || exit 1
    git push || exit 1
    popd
fi

nix flake update --recreate-lock-file

if [ -z "${OVERRIDE:-}" ]
then
    su -c "nixos-rebuild --flake . switch"
else
    su -c "nixos-rebuild --flake . --override-input nixpkgs ../nixpkgs --no-write-lock-file switch"
fi
