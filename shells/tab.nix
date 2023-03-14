{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "Tab";
  imports = [ "${inputs.devshell}/extra/language/c.nix" ];
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
  env = [
    { name = "GEM_HOME"; eval = "$PRJ_DATA_DIR/bundle/$(ruby -e 'puts RUBY_VERSION')"; }
    { name = "PATH"; prefix = "$GEM_HOME/bin"; }
  ];
  serviceGroups.server.services = {
    web.command = "rails s -p 3000";
    js.command = "yarn build:dev --watch";
    css.command = "yarn build:css --watch";
  };
  packages = with pkgs; [
    (pkgs.lowPrio binutils)
    findutils
    gnumake
    ruby_3_1
    nodejs
    yarn
  ];
  language.c = {
    compiler = pkgs.gcc;
    includes = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
    libraries = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
  };
}
