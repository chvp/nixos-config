{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.eid-mw ];

  home-manager.users.charlotte = { ... }: {
    programs.qutebrowser = {
      enable = true;
      keyBindings = {
        normal = {
          "x" = "spawn --detach mpv {url}";
          ";x" = "hint links spawn --detach mpv {hint-url}";
        };
      };
      extraConfig = ''
        config.load_autoconfig()
      '';
    };
  };

  custom.zfs.homeLinks = [
    { path = ".pki"; type = "cache"; } # Required for eid-mw browser configuration
    { path = ".cache/qutebrowser"; type = "cache"; }
    { path = ".local/share/qutebrowser"; type = "data"; }
    { path = ".config/qutebrowser/autoconfig.yml"; type = "data"; }
  ];
}
