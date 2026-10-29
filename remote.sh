#!/usr/bin/env bash
set -euo pipefail
set -x

hostname=$1
shift 1

if [ "$1" == "cache" ]
then
    nix build -L --no-link --print-out-paths .#nixosConfigurations.$hostname.config.system.build.toplevel | xargs attic push chvp
else
    nixos-rebuild --flake .#$hostname --target-host root@$hostname -s "$@"
fi
