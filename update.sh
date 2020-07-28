#!/usr/bin/env bash
set -euo pipefail
set -x

BUILD_ARGS=(
    "--extra-substituters"
    "ssh://charlotte@sunspear.vanpetegem.me"
    "--trusted-public-keys"
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= sunspear-nix-cache:4mgL4qS7EweCug1gAFiZKgQK+xuoJMBYThIUE+kPX4s="
)

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
