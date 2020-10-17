{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.eid-mw ];

  home-manager.users.charlotte = { ... }: {
    programs.qutebrowser = {
      enable = true;
      keyBindings = {
        normal = {
          "x" = "spawn --detach mpv {url}";
          ";x" = "hint links spawn --detach mpv {hint-url}";
          "tch" = "config-cycle -p -t -u *://{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
          "tCh" = "config-cycle -p -u *://{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
          "tcH" = "config-cycle -p -t -u *://*.{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
          "tCH" = "config-cycle -p -u *://*.{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
          "tcu" = "config-cycle -p -t -u {url} content.cookies.accept no-3rdparty never ;; reload";
          "tCu" = "config-cycle -p -u {url} content.cookies.accept no-3rdparty never ;; reload";
        };
      };
      settings = {
        auto_save.session = true;
        content = {
          autoplay = false;
          cookies.accept = "never";
          default_encoding = "utf-8";
          geolocation = false;
          javascript.enabled = false;
          pdfjs = true;
        };
        downloads = {
          location.directory = "~/downloads";
          open_dispatcher = "rifle";
          remove_finished = 1000;
        };
        editor.command = [ "kitty" "-e" "nvim" "{file}" "-c" "normal {line}G{column0}" ];
        input.insert_mode.auto_load = true;
        url.default_page = "about:blank";
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
