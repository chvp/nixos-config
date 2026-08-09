{ pkgs, inputs, ... }:
pkgs.devshell.mkShell {
  name = "Tap";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = with pkgs; [
    nodejs_24
    yarn
    sqlite
    imagemagick
  ];
  language.ruby = {
    package = pkgs.ruby_4_0;
    nativeDeps = [ pkgs.zlib pkgs.libffi pkgs.libyaml pkgs.libmysqlclient ];
  };
}
