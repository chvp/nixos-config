#!/usr/bin/env bash
set -euo pipefail
set -x

nix flake update

BUILD_ARGS=(
    "--extra-substituters"
    "ssh://charlotte@sunspear.vanpetegem.me"
    "--trusted-public-keys"
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= sunspear-nix-cache:4mgL4qS7EweCug1gAFiZKgQK+xuoJMBYThIUE+kPX4s="
)

if [ -d "../nixpkgs" -a -z "${NO_LOCAL:-}" ]
then
    BUILD_ARGS+=("--override-input" "nixpkgs" "../nixpkgs" "--no-write-lock-file")
fi

sudo nix build --profile /nix/var/nix/profiles/system ".#nixosConfigurations.$(hostname).config.system.build.toplevel" --no-link "${BUILD_ARGS[@]}" && \
    sudo nix --experimental-features "nix-command flakes" shell -vv /nix/var/nix/profiles/system -c switch-to-configuration switch
