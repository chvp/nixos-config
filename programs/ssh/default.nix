{ ... }:

{
  home-manager.users.charlotte = { ... }: {
    programs.ssh = {
      enable = true;
      compression = true;
      hashKnownHosts = true;
      serverAliveInterval = 300;
    };
  };
}
