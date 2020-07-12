#!/usr/bin/env bash
set -euo pipefail
set -x

nix-build \
    --no-out-link \
    --pure \
    --builders 'ssh://charlotte@sunspear.vanpetegem.me' \
    --extra-substituters 'ssh://charlotte@sunspear.vanpetegem.me' \
    "${@}"

exit 0
