{ pkgs, lib, inputs, system, ... }: pkgs.devshell.mkShell {
  name = "NixOS config";
  commands = [
    {
      name = "format-all";
      category = "general commands";
      help = "Format all nix files in the project";
      command = "find $PRJ_ROOT -type f -name '*.nix' -print0 | xargs -0 nixpkgs-fmt";
    }
    {
      name = "fetchpatch";
      category = "general commands";
      help = "Fetch a patch from a nixpkgs PR by its ID";
      command = "curl -L https://github.com/NixOS/nixpkgs/pull/$1.diff -o $PRJ_ROOT/patches/$1.patch";
    }
  ];
  packages = [
    pkgs.agenix
    pkgs.nixos-rebuild
    pkgs.nixpkgs-fmt
  ] ++ lib.optional pkgs.stdenv.isDarwin inputs.darwin.packages.${system}.default;
}
