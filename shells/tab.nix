{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "Tab";
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
  serviceGroups.server.services = {
    web.command = "rails s -p 3000";
    js.command = "yarn build:dev --watch";
    css.command = "yarn build:css --watch";
  };
  packages = with pkgs; [
    nodejs
    yarn
  ];
  language.ruby = {
    package = pkgs.ruby_3_1;
    nativeDeps = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
  };
}
