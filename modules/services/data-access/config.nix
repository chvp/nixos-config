{ pkgs, ... }:

{
  users.users.data = {
    isNormalUser = true;
    home = "/home/data";
    description = "Data Access";
    uid = 1000;
    group = "users";
    passwordFile = "/run/secrets/password_file";
  };
  environment.systemPackages = [ pkgs.rsync pkgs.mktorrent (pkgs.writeShellScriptBin "create_torrent" ". /run/secrets/create_torrent") ];
  security.sudo.enable = false;
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    hostKeys = [
      { bits = 4096; path = "/run/secrets/ssh_host_rsa_key"; type = "rsa"; }
      { path = "/run/secrets/ssh_host_ed25519_key"; type = "ed25519"; }
    ];
    authorizedKeysFiles = [ "/run/secrets/authorized_keys" ];
  };
}
