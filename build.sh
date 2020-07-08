#!/usr/bin/env bash
set -euo pipefail
set -x

nix-build \
    --no-out-link \
    --pure \
    "${@}"

exit 0
