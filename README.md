# NixOS config

## Setting up a new dev environment

* Create a new `*.nix` file in the shells directory that describes the environment (this is the hard part).

* Execute `use_nix > .envrc` to initialize the `.envrc` file.

* Execute `ln -s /path/to/correct/file.nix shell.nix`.

* Execute `direnv allow` to load the `.envrc` file which in turn loads your environment.
