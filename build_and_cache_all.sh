#!/usr/bin/env bash
set -euo pipefail
set -x

FAILED=0

for system in $(ls machines)
do
    if ! nix build -L --no-link --print-out-paths ".#nixosConfigurations.$system.config.system.build.toplevel" | xargs attic push chvp
    then
        FAILED=1
    fi
done

for shell in $(ls shells)
do
    shell="${shell%.nix}"
    if ! nix build -L --no-link --print-out-paths ".#devShells.x86_64-linux.$shell" | xargs attic push chvp
    then
        FAILED=1
    fi
done

exit $FAILED
