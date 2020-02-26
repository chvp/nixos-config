{ ... }:

{
  home-manager.users.charlotte = { ... }: {
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
