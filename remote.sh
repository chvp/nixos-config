#!/usr/bin/env bash
set -euo pipefail
set -x

hostname=$1
shift 1

if [ "$1" == "cache" ]
then
    nix build -L --no-link .#nixosConfigurations.$hostname.config.system.build.toplevel
    nix eval --raw ".#nixosConfigurations.$hostname.config.system.build.toplevel.outPath" | attic push chvp --stdin
else
    nixos-rebuild --flake .#$hostname --target-host root@$hostname -s "$@"
fi
