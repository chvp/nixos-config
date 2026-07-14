#!/usr/bin/env bash
set -euo pipefail
set -x

FAILED=0

for system in $(ls machines)
do
    if nix build -L --no-link ".#nixosConfigurations.$system.config.system.build.toplevel"
    then
        nix eval --raw ".#nixosConfigurations.$system.config.system.build.toplevel.outPath" | attic push chvp --stdin
    else
        FAILED=1
    fi
done

for shell in $(ls shells)
do
    shell="${shell%.nix}"
    if nix build -L --no-link ".#devShells.x86_64-linux.$shell"
    then
        nix eval --raw ".#devShells.x86_64-linux.$shell.outPath" | attic push chvp --stdin
    else
        FAILED=1
    fi
done

exit $FAILED
