# Packaging node packages

To package a node package, add it to the array in `packages.json`. Then execute `nix-shell -p nodePackages.node2nix --run "node2nix --node12 -i packages.json"`.
