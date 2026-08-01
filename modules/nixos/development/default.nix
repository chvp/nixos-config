{ config, inputs, lib, pkgs, ... }:

{
  imports = [
    ./android
    ./docker
  ];

  config = lib.mkIf config.chvp.development.enable {
    chvp = {
      base = {
        emacs.extraPackages = [
          (epkgs: [ epkgs.treesit-grammars.with-all-grammars ])
        ];
        zfs.homeLinks = [{ path = "repos"; type = "cache"; }];
      };
      development.docker.enable = lib.mkDefault true;
    };

    home-manager.users.charlotte = { ... }: {
      home.file.".ideavimrc".text = ''
        set clipboard+=unnamedplus,ideaput
        set ideajoin
      '';
    };

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;

    users.users.charlotte.extraGroups = [ "vboxusers" ];
  };
}
