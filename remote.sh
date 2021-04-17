#!/usr/bin/env bash
set -euo pipefail
set -x

hostname=$1
shift 1

nixos-rebuild --flake .#$hostname --target-host root@$hostname "$@"
