{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "NixOS config";
  packages = [
    pkgs.nixpkgs-fmt
    (pkgs.writeShellScriptBin "fetchpatch" "curl -L https://github.com/NixOS/nixpkgs/pull/$1.patch -o patches/$1.patch")
    inputs.agenix.defaultPackage.x86_64-linux
  ];
}
