{
  devshell,
  inputs,
  libffi,
  libyaml,
  nodejs,
  ruby_4_0,
  zlib,
}:

devshell.mkShell {
  name = "Basic ruby project";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = [ nodejs ];
  language.ruby = {
    package = ruby_4_0;
    nativeDeps = [
      zlib
      libffi
      libyaml
    ];
  };
}
