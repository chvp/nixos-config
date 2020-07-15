#!/usr/bin/env bash
set -euo pipefail
set -x

./update-imports.sh

BUILD_ARGS=(
    "--builders"
    "ssh://charlotte@sunspear.vanpetegem.me"
    "--extra-substituters"
    "ssh://charlotte@sunspear.vanpetegem.me"
    "--trusted-public-keys"
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= sunspear-nix-cache:4mgL4qS7EweCug1gAFiZKgQK+xuoJMBYThIUE+kPX4s="
)
result="$(./build.sh "${BUILD_ARGS[@]}" "./machines/$(hostname)")"

sudo bash -c "nix-env --set --profile /nix/var/nix/profiles/system/ ${result} && ${result}/bin/switch-to-configuration switch"
