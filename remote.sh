#!/usr/bin/env bash
set -euo pipefail
set -x

nix build --no-link --profile /nix/var/nix/profiles/per-user/charlotte/$1 .#nixosConfigurations.$1.config.system.build.toplevel
nixos-rebuild --flake .#$1 --target-host root@$1 --build-host localhost $2
