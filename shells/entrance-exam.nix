{ pkgs, inputs, ... }:
let
  support-services = {
    css.command = "yarn build:css --watch";
    js.command = "yarn build:js --watch=forever";
  };
  all-services = support-services // {
    rails = {
      name = "server";
      command = "rails s -p 3000";
    };
  };
in
pkgs.devshell.mkShell {
  name = "Entrance exam";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = with pkgs; [
    chromedriver
    ungoogled-chromium
    nodejs
    nodePackages.typescript-language-server
    yarn
    bundix
  ];
  commands = [
    {
      name = "refresh-deps";
      category = "general commands";
      help = "Install dependencies";
      command = ''
        yarn install
        bundle install
        bundle pristine
      '';
    }
  ];
  serviceGroups = {
    server.services = all-services;
    server-support.services = support-services;
  };
  language.ruby = {
    package = pkgs.ruby_3_4;
    nativeDeps = [ pkgs.openssl pkgs.zlib pkgs.libffi pkgs.libyaml ];
  };
}
