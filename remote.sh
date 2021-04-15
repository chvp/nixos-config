#!/usr/bin/env bash
set -euo pipefail
set -x

drv=$(nix eval .#nixosConfigurations.$1.config.system.build.toplevel.drvPath | tr -d '"')
nix copy --derivation -s --to "ssh://root@$1" "$drv"
ssh root@$1 nix build -L --no-link "$drv"
nixos-rebuild --flake .#$1 --target-host root@$1 $2
