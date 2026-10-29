#!/usr/bin/env bash
set -euo pipefail

FAILED=0

build_and_cache() {
    echo "################################################################################"
    echo 🔧 ️Building and caching $1
    nix build -L --no-link --print-out-paths ".#$1" | xargs attic push chvp
}

for system in $(ls machines)
do
    if ! build_and_cache "nixosConfigurations.$system.config.system.build.toplevel"
    then
        FAILED=1
    fi
done

for shell in $(ls shells)
do
    if ! build_and_cache "devShells.x86_64-linux.${shell%.nix}"
    then
        FAILED=1
    fi
done

exit $FAILED
