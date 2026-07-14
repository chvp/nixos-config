#!/usr/bin/env bash
set -euo pipefail
set -x

FAILED=0

for system in $(ls machines)
do
    if nix build -L --no-link ".#nixosConfigurations.$system.config.system.build.toplevel"
    then
        nix eval --json ".#nixosConfigurations.$system.config.system.build.toplevel.outPath" | sed 's/"\(.*\)"/\1/' | attic push chvp --stdin
    else
        FAILED=1
    fi
done

for shell in $(ls shells)
do
    shell="${shell%.nix}"
    if nix build -L --no-link ".#devShells.x86_64-linux.$shell"
    then
        nix eval --json ".#devShells.x86_64-linux.$shell.outPath" | sed 's/"\(.*\)"/\1/' | attic push chvp --stdin
    else
        FAILED=1
    fi
done

exit $FAILED
