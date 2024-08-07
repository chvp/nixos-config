{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
  phone-push = pkgs.writeShellScriptBin "phone-push" ''
    curl $(cat ${config.age.secrets."files/services/phone-push-url".path}) -d "$(hostname): $@"
  '';
in
{
  environment.systemPackages = [ phone-push ];

  age.secrets."files/services/phone-push-url" = {
    file = ../../../../secrets/files/services/phone-push-url.age;
    owner = username;
  };
}
