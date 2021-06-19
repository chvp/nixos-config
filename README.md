# NixOS config

## Secrets

There are two types of secrets in this repository. Secret secrets, and
secret configuration.

Secret secrets should never be world-readable, even to users who are
logged in to one of the hosts managed by this configuration. These are
generally managed by agenix, allowing them to still be put in the nix
store.

Secret configuration is generally more security through obscurity
(e.g. some services that I run that I don't want the whole world to
know what ports they run on). These are managed with git-crypt and are
files that end in `secret.nix`.

## Setting up a new dev environment

* Create a new `*.nix` file in the shells directory that describes the environment (this is the hard part).

* Execute `use_nix > .envrc` to initialize the `.envrc` file.

* Execute `ln -s /path/to/correct/file.nix shell.nix`.

* Execute `direnv allow` to load the `.envrc` file which in turn loads your environment.
