#!/usr/bin/env bash
set -euo pipefail
set -x

BUILD_ARGS=()

if [ -d "../nixpkgs" -a -z "${NO_LOCAL:-}" ]
then
    BUILD_ARGS+=("--override-input" "nixpkgs" "../nixpkgs" "--no-write-lock-file")
    pushd ../nixpkgs
    git fetch --all --prune
    git rebase upstream/nixos-unstable-small || exit 1
    git push || exit 1
    popd
fi

nix flake update --update-input nixpkgs --update-input home-manager --update-input flake-utils

sudo nix build --profile /nix/var/nix/profiles/system ".#nixosConfigurations.$(hostname).config.system.build.toplevel" --no-link "${BUILD_ARGS[@]}" && \
    sudo nix --experimental-features "nix-command flakes" shell -vv /nix/var/nix/profiles/system -c switch-to-configuration switch
