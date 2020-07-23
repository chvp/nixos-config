#!/usr/bin/env bash
set -euo pipefail
set -x

nix flake update

BUILD_ARGS=()

if [ -d "../nixpkgs" -a -z "${NO_LOCAL:-}" ]
then
    BUILD_ARGS+=("--override-input" "nixpkgs" "../nixpkgs" "--no-write-lock-file")
fi

sudo nix build --profile /nix/var/nix/profiles/system ".#nixosConfigurations.$(hostname).config.system.build.toplevel" --no-link "${BUILD_ARGS[@]}" && \
    sudo nix --experimental-features "nix-command flakes" shell -vv /nix/var/nix/profiles/system -c switch-to-configuration switch
