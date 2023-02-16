{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "Tap";
  imports = [ "${inputs.devshell}/extra/language/c.nix" ];
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
  env = [
    { name = "CC"; value = "cc"; }
    { name = "CPP"; value = "cpp"; }
    { name = "CXX"; value = "c++"; }
    { name = "GEM_HOME"; eval = "$PRJ_DATA_DIR/bundle/$(ruby -e 'puts RUBY_VERSION')"; }
    { name = "PATH"; prefix = "$GEM_HOME/bin"; }
  ];
  commands = [
    {
      name = "server-support";
      category = "general commands";
      help = "Run everything required for a server";
      command = ''
        bundle install
      '';
    }
    {
      name = "server";
      category = "general commands";
      help = "Run everything";
      command = ''
        server-support
        rails s
      '';
    }
  ];
  language.c = {
    compiler = pkgs.gcc;
    includes = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
    libraries = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
  };
}
