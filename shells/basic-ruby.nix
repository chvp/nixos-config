{ pkgs, inputs, ... }:
pkgs.devshell.mkShell {
  name = "Basic ruby project";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = with pkgs; [
    nodejs
    nodePackages.typescript-language-server
    rubyPackages_3_4.solargraph
    yarn
  ];
  language.ruby = {
    package = pkgs.ruby_3_4;
    nativeDeps = [ pkgs.zlib pkgs.libffi pkgs.libyaml ];
  };
}
