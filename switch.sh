#!/usr/bin/env bash

sudo nix shell -vv /nix/var/nix/profiles/system -c switch-to-configuration switch
