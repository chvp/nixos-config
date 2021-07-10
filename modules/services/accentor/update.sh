#!/usr/bin/env nix-shell
#!nix-shell -p curl yarn2nix bundix -i bash
curl -L -O https://github.com/accentor/api/raw/develop/Gemfile
curl -L -O https://github.com/accentor/api/raw/develop/Gemfile.lock
curl -L -O https://github.com/accentor/web/raw/develop/package.json
curl -L -O https://github.com/accentor/web/raw/develop/yarn.lock
yarn2nix --lockfile yarn.lock --no-patch > yarn.nix
bundix -l
