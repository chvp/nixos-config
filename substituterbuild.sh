#!/usr/bin/env bash
set -euo pipefail
set -x

export NO_REMOTE=true
for machine in machines/*
do
    output=$(./build.sh $machine)
    nix-env --set -p /nix/var/nix/profiles/per-user/charlotte/${machine#machines/} $output
done
