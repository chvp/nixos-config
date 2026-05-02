#!/usr/bin/env bash
set -euo pipefail
set -x

hostname=$1
shift 1

if [ "$1" == "cache" ]
then
    nix build -L --no-link .#nixosConfigurations.$hostname.config.system.build.toplevel
    nix eval --json ".#nixosConfigurations.$hostname.config.system.build.toplevel.outPath" | sed 's/"\(.*\)"/\1/' | attic push chvp --stdin
else
    nixos-rebuild --flake .#$hostname --target-host root@$hostname -s "$@"
fi
