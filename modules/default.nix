{ config, lib, pkgs, ... }:

{
  imports = [
    ./accentor.nix
    ./android.nix
    ./bluetooth.nix
    ./calibre.nix
    ./docker.nix
    ./deluge-client.nix
    ./deluge-server.nix
    ./dropbox.nix
    ./eid.nix
    ./emacs.nix
    ./firefox.nix
    ./git.nix
    ./global-mailer.nix
    ./gnupg.nix
    ./graphical.nix
    ./hledger.nix
    ./mail-client.nix
    ./minecraft.nix
    ./mumble.nix
    ./networkmanager.nix
    ./nextcloud.nix
    ./nix.nix
    ./nginx.nix
    ./obs.nix
    ./ovh.nix
    ./pass.nix
    ./smartd.nix
    ./sound.nix
    ./ssh.nix
    ./sshd.nix
    ./steam.nix
    ./sway
    ./syncthing-client.nix
    ./syncthing-server.nix
    ./teeworlds.nix
    ./terminal.nix
    ./tetris.nix
    ./theming.nix
    ./tmux.nix
    ./ugent
    ./xdg.nix
    ./zeroad.nix
    ./zfs.nix
    ./zotero.nix
    ./zsh.nix
  ];

  options.chvp = {
    stateVersion = lib.mkOption {
      example = "20.09";
    };

    dataPrefix = lib.mkOption {
      default = "";
      example = "/data";
    };

    cachePrefix = lib.mkOption {
      default = "";
      example = "/cache";
    };

    hasContainers = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = {
    home-manager.useGlobalPkgs = true;

    system.stateVersion = config.chvp.stateVersion;
    home-manager.users = {
      charlotte = { ... }: {
        home.stateVersion = config.chvp.stateVersion;
      };
      root = { ... }: {
        home.stateVersion = config.chvp.stateVersion;
      };
    };

    environment.systemPackages = with pkgs; [ htop moreutils ncdu ripgrep sshfs unzip ];

    console = {
      colors = [
        "f8f8f8"
        "a60000"
        "005e00"
        "813e00"
        "0031a9"
        "721045"
        "00538b"
        "282828"
        "ffffff"
        "972500"
        "315b00"
        "70480f"
        "2544bb"
        "8f0075"
        "30517f"
        "000000"
      ];
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    i18n = {
      defaultLocale = "en_IE.UTF-8";
      extraLocaleSettings = {
        LC_TIME = "en_GB.UTF-8";
      };
    };

    networking.nat = lib.mkIf config.chvp.hasContainers {
      enable = true;
      enableIPv6 = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "eno3";
    };

    security.sudo.enable = false;
    security.doas = {
      enable = true;
      extraRules = [
        {
          users = [ "charlotte" ];
          noPass = true;
          cmd = "nix-collect-garbage";
          runAs = "root";
        }
      ];
    };

    services.fwupd.enable = true;

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users = {
        charlotte = {
          isNormalUser = true;
          home = "/home/charlotte";
          description = "Charlotte Van Petegem";
          extraGroups = [ "systemd-journal" ];
          passwordFile = config.age.secrets."passwords/users/charlotte".path;
        };
        root.passwordFile = config.age.secrets."passwords/users/root".path;
      };
    };

    age.secrets = {
      "passwords/users/charlotte".file = ../secrets/passwords/users/charlotte.age;
      "passwords/users/root".file = ../secrets/passwords/users/root.age;
    };
  };
}
