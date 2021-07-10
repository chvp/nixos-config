#!/usr/bin/env nix-shell
#!nix-shell -p curl yarn2nix -i bash
curl -L -O https://github.com/chvp/tetris/raw/master/package.json
curl -L -O https://github.com/chvp/tetris/raw/master/yarn.lock
yarn2nix --lockfile yarn.lock --no-patch > yarn.nix
