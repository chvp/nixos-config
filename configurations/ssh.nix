{ ... }:

{
  imports = [ ./ssh/secret.nix ];

  custom.zfs.homeLinks = [
    { path = ".ssh/known_hosts"; type = "cache"; }
  ];

  nixpkgs.overlays = [
    (self: super: {
      ssh = self.symlinkJoin {
        name = "openssh";
        paths = [
          (
            self.writeScriptBin "ssh" ''
              #!${self.zsh}/bin/zsh

              export TERM=xterm-256color
              ${super.openssh}/bin/ssh $@
            ''
          )
          super.openssh
        ];
      };
    })
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      ssh
    ];
    programs.ssh = {
      enable = true;
      compression = true;
      hashKnownHosts = true;
      serverAliveInterval = 300;
      extraConfig = ''
        HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,rsa-sha2-512-cert-v01@openssh.com,rsa-sha2-256-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,rsa-sha2-512,rsa-sha2-256,ssh-rsa
      '';
    };
  };
}
