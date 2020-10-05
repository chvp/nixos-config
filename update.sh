#!/usr/bin/env bash
set -euo pipefail
set -x

if [ -d "../nixpkgs" -a -z "${NO_LOCAL:-}" ]
then
    pushd ../nixpkgs
    git fetch --all --prune
    git rebase upstream/nixos-unstable || exit 1
    git push || exit 1
    popd
fi

nix flake update --update-input nixpkgs --update-input home-manager --update-input flake-utils

./build.sh && ./switch.sh
