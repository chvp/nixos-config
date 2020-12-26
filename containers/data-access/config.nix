{ pkgs, ... }: {

  imports = [
    ./config.secret.nix
  ];

  users.users.data = {
    isNormalUser = true;
    home = "/home/data";
    description = "Data Access";
    uid = 1000;
    group = "users";
  };
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    hostKeys = [
      { bits = 4096; path = "/var/secrets/ssh_host_rsa_key"; type = "rsa"; }
      { path = "/var/secrets/ssh_host_ed25519_key"; type = "ed25519"; }
    ];
  };
}
