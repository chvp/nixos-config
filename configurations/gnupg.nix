{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".gnupg/crls.d"; type = "data"; }
    { path = ".gnupg/private-keys-v1.d"; type = "data"; }
    { path = ".gnupg/pubring.kbx"; type = "data"; }
    { path = ".gnupg/trustdb.gpg"; type = "data"; }
  ];
  programs.gnupg.agent.enable = true;
  home-manager.users.charlotte = { pkgs, ... }: {
    programs = {
      gpg.enable = true;
    };
    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 7200;
      maxCacheTtl = 99999;
      pinentryFlavor = "qt";
    };
  };
}
