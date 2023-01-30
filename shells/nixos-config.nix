{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "NixOS config";
  packages = [
    # Use nixos-rebuild from flake, since it might be patched
    pkgs.nixos-rebuild
    pkgs.nixpkgs-fmt
    (pkgs.writeShellScriptBin "fetchpatch" "curl -L https://github.com/NixOS/nixpkgs/pull/$1.patch -o patches/$1.patch")
    inputs.agenix.packages.${pkgs.system}.default
  ];
}
