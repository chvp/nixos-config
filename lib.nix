{
  findImport = name:
    let
      localpath = ./.. + "/${name}";
      importpath = ./imports + "/${name}";
    in
    if builtins.pathExists localpath then
      localpath
    else if builtins.pathExists importpath then
      (import importpath)
    else (abort "couldn't find import ${name}");

  mkSystem = { nixpkgs, system ? "x86_64-linux", rev ? "git", extraModules ? [ ], ... }:
    let
      machine = import "${nixpkgs}/nixos/lib/eval-config.nix" {
        inherit system;
        modules = [
          ({ ... }: {
            nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
          })
        ] ++ extraModules;
      };
    in
    machine;
}
