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
  users.users.readonly = {
    isNormalUser = true;
    home = "/home/readonly";
    description = "Readonly data access";
    uid = 1001;
    group = "sftponly";
    passwordFile = "/run/secrets/readonly_password_file";
  };
  users.groups.sftponly = { gid = 10000; };
  environment.systemPackages = [ pkgs.rsync pkgs.mktorrent (pkgs.writeShellScriptBin "create_torrent" ". /run/secrets/create_torrent") ];
  security.sudo.enable = false;
  services.openssh = {
    enable = true;
    hostKeys = [
      { bits = 4096; path = "/run/secrets/ssh_host_rsa_key"; type = "rsa"; }
      { path = "/run/secrets/ssh_host_ed25519_key"; type = "ed25519"; }
    ];
    settings = {
      HostKeyAlgorithms = "+ssh-rsa";
      Macs = [ "hmac-sha2-512-etm@openssh.com" "hmac-sha2-256-etm@openssh.com" "umac-128-etm@openssh.com" "hmac-sha-512" ];
      PermitRootLogin = "no";
    };
    extraConfig = ''
      Match group sftponly
          X11Forwarding no
          AllowTcpForwarding no
          AllowAgentForwarding no
          ForceCommand internal-sftp
      Match user data
          PasswordAuthentication no
          KbdInteractiveAuthentication no
    '';
    authorizedKeysFiles = [ "/run/secrets/%u_authorized_keys" ];
  };
}
