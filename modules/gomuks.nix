{ config, lib, pkgs, ... }:
let cfg = config.chvp;
in
{
  options.chvp.gomuks.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.gomuks.enable {
    nixpkgs.overlays = [
      (self: super: {
        gomuks = pkgs.writeShellScriptBin "gomuks" ''
          export GOMUKS_CACHE_HOME="${cfg.cachePrefix}${config.users.users.charlotte.home}/.cache/gomuks"
          export GOMUKS_DATA_HOME="${cfg.cachePrefix}${config.users.users.charlotte.home}/.local/share/gomuks"
          export GOMUKS_CONFIG_HOME="${cfg.cachePrefix}${config.users.users.charlotte.home}/.config/gomuks"
          ${super.alacritty}/bin/alacritty -e ${super.gomuks}/bin/gomuks
        '';
      })
    ];

    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.gomuks ];
    };
  };
}
