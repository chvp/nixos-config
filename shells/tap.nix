{ pkgs, inputs, ... }: pkgs.devshell.mkShell {
  name = "Tap";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
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
  packages = with pkgs; [
    imagemagick
    nodejs
    yarn
  ];
  serviceGroups.server.services.rails = {
    name = "server";
    command = "rails s -p 3000";
  };
  language.ruby = {
    package = pkgs.ruby_3_0;
    nativeDeps = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
  };
}
