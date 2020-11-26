#!/usr/bin/env bash
set -euo pipefail
set -x

nixos-rebuild --flake .#$1 --target-host root@$1 --build-host localhost $2
