#!/usr/bin/env bash
set -euo pipefail
set -x

./update-imports.sh

result="$(./build.sh "./machines/$(hostname)")"

sudo bash -c "nix-env --set --profile /nix/var/nix/profiles/system/ ${result} && ${result}/bin/switch-to-configuration switch"
