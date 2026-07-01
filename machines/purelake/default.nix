# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Brussels";

  chvp = {
    stateVersion = "24.11";
    base = {
      bluetooth.enable = true;
      network.mobile = {
        enable = true;
        wireless-interface = "wlp8s0";
        wired-interfaces = {
          "enp9s0" = { };
        };
      };
      nix.unfreePackages = [ "nvidia-kernel-modules" "nvidia-settings" "nvidia-x11" "google-chrome" ];
    };
    games.enable = true;
    graphical.enable = false;
  };

  # Enable the X11 windowing system.
  services.xserver.videoDrivers = [ "nvidia" ];

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

  programs.kdeconnect.enable = true;

  services.xserver.xkb = {
    layout = "us";
    variant = "altgr-intl";
  };

  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        ControllerMode = "dual";
        JustWorksRepairing = "confirm";
      };
      LE = {
        MinConnectionInterval = 7;
        MaxConnectionInterval = 9;
        ConnectionLatency = 0;
      };
    };
  };
  hardware.xpadneo.enable = true;

  # Enable sound with pipewire.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  nixpkgs.overlays = [
    (self: super: {
      linux-firmware = super.linux-firmware.overrideAttrs (old: rec {
        version = "20250509";
        src = pkgs.fetchFromGitLab {
          owner = "kernel-firmware";
          repo = "linux-firmware";
          tag = version;
          hash = "sha256-0FrhgJQyCeRCa3s0vu8UOoN0ZgVCahTQsSH0o6G6hhY=";
        };
      });
    })
  ];

  users.users.charlotte = {
    isNormalUser = true;
    description = "Charlotte Van Petegem";
    extraGroups = [ "networkmanager" "wheel" "video" "render" "vboxusers" ];
  };

  home-manager.users.charlotte = { ... }: {
    services.network-manager-applet.enable = lib.mkForce false;
  };

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "charlotte";

  programs.firefox.enable = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    google-chrome
    wine
    protontricks
    sshfs
    mpv
    yt-dlp
  ];

  programs = {
    fuse.enable = lib.mkForce true;
    steam = {
      enable = true;
      protontricks.enable = true;
    };
  };

  fileSystems."/data" = {
    device = "readonly@data.vanpetegem.me:data";
    fsType = "sshfs";
    options = [
      "nodev"
      "noatime"
      "allow_other"
      "delay_connect"
      "reconnect"
      "_netdev"
      "port=2002"
      "IdentityFile=/root/.ssh/id_ed25519"
    ];
  };
}
