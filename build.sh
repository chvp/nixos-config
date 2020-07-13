#!/usr/bin/env bash
set -euo pipefail
set -x

if [ -z "${NO_REMOTE:-}" ]
then
    remote_args="--builders ssh://charlotte@sunspear.vanpetegem.me --extra-substituters ssh://charlotte@sunspear.vanpetegem.me"
else
    remote_args=""
fi


nix-build \
    --no-out-link \
    --pure \
    $remote_args \
    "${@}"

exit 0
