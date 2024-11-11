{ config, lib, ... }:

# Define shared options so that links configuration can live close to actual configuration
{
  options.chvp.base.zfs = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    encrypted = lib.mkOption {
      default = false;
      example = true;
    };
    systemLinks = lib.mkOption {
      default = [ ];
      example = [
        { path = "/var/lib/docker"; type = "cache"; }
        { path = "/var/lib/docker/volumes"; type = "data"; }
      ];
    };
    homeLinks = lib.mkOption {
      default = [ ];
      example = [
        { path = ".config/syncthing"; type = "data"; }
        { path = ".cache/nix-index"; type = "cache"; }
      ];
    };
    ensureSystemExists = lib.mkOption {
      default = [ ];
      example = [ "/data/etc/ssh" ];
    };
    ensureHomeExists = lib.mkOption {
      default = [ ];
      example = [ ".ssh" ];
    };
    backups = lib.mkOption {
      default = [ ];
      example = [{
        path = "rpool/safe/data";
        remotePath = "zdata/recv/<hostname>/safe/data";
        fast = false;
        location = "marabethia.vanpetegem.me";
      }];
    };
    rootDataset = lib.mkOption {
      example = "rpool/local/root";
    };
    rootPool = lib.mkOption {
      example = "rpool";
    };
  };
}
