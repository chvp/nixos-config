#!/usr/bin/env bash
set -euo pipefail
set -x

BUILD_ARGS=("--secret-key-files" "/etc/nix/key.private")
for machine in machines/*
do
    output=$(./build.sh "${BUILD_ARGS[@]}" "$machine")
    nix-env --set -p /nix/var/nix/profiles/per-user/charlotte/${machine#machines/} $output
done
