{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "Tap";
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
  packages = with pkgs; [
    (pkgs.lowPrio binutils)
    imagemagick
    file
    findutils
    gnumake
    ruby_3_0
    nodejs
    yarn
  ];
  serviceGroups.server.services.rails = {
    name = "server";
    command = "rails s -p 3000";
  };
  language.c = {
    compiler = pkgs.gcc;
    includes = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
    libraries = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
  };
}
