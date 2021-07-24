#!/usr/bin/env nix-shell
#!nix-shell -p curl yarn2nix bundix -i bash
curl -L -O https://github.com/accentor/api/raw/main/Gemfile
curl -L -O https://github.com/accentor/api/raw/main/Gemfile.lock
curl -L -O https://github.com/accentor/web/raw/main/package.json
curl -L -O https://github.com/accentor/web/raw/main/yarn.lock
yarn2nix --lockfile yarn.lock --no-patch > yarn.nix
bundix -l
