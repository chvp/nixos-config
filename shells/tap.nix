{ devshell
, inputs
, chromedriver
, imagemagick
, libffi
, libmysqlclient
, libyaml
, nodejs_24
, ruby_4_0
, sqlite
, ungoogled-chromium
, zlib
}:

devshell.mkShell {
  name = "Tap";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = [
    chromedriver
    imagemagick
    nodejs_24
    sqlite
    ungoogled-chromium
  ];
  language.ruby = {
    package = ruby_4_0;
    nativeDeps = [ zlib libffi libyaml libmysqlclient ];
  };
}
