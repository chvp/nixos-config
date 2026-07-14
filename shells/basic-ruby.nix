{ pkgs, inputs, ... }:
pkgs.devshell.mkShell {
  name = "Basic ruby project";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = with pkgs; [
    nodejs
    yarn
  ];
  language.ruby = {
    package = pkgs.ruby_4_0;
    nativeDeps = [ pkgs.zlib pkgs.libffi pkgs.libyaml ];
  };
}
