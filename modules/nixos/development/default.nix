{ config, inputs, lib, pkgs, ... }:

{
  imports = [
    ./android
    ./docker
  ];

  config = lib.mkIf config.chvp.development.enable {
    chvp = {
      base = {
        nix.unfreePackages = [ "ruby-mine" "ruby-mine-with-plugins" "virtualbox-extpack" ];
        zfs.homeLinks = [
          { path = "repos"; type = "cache"; }
          { path = ".config/JetBrains"; type = "cache"; }
          { path = ".config/github-copilot"; type = "cache"; }
          { path = ".local/share/JetBrains"; type = "cache"; }
          { path = ".cache/JetBrains"; type = "cache"; }
          { path = ".java/.userPrefs"; type = "cache"; }
          { path = "VirtualBox VMs"; type = "cache"; }
          { path = ".config/VirtualBox"; type = "cache"; }
        ];
      };
      development.docker.enable = lib.mkDefault true;
    };

    home-manager.users.charlotte = { ... }: {
      home.packages = [ (inputs.nix-jetbrains-plugins.lib.buildIdeWithPlugins pkgs "ruby-mine" [ "com.github.copilot" "IdeaVIM" ]) ];
      home.file.".ideavimrc".text = ''
        set clipboard+=unnamedplus,ideaput
        set ideajoin
      '';
    };

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;

    users.users.charlotte.extraGroups = [ "vboxusers" ];

    virtualisation.virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };
}
