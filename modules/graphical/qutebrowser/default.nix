{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.qutebrowser.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.qutebrowser.enable {
    nixpkgs.overlays = [
      (self: super: {
        qutebrowser = super.qutebrowser.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [
            (self.fetchpatch {
              url = "https://github.com/qutebrowser/qutebrowser/pull/6626.patch";
              sha256 = "aWhkUQ2eaLNiQwxRTbzfFrM0TrDSKtPKinjGu/RknLk=";
            })
          ];
        });
      })
    ];
    chvp.base.zfs.homeLinks = [
      { path = ".config/qutebrowser"; type = "data"; }
      { path = ".local/share/qutebrowser"; type = "cache"; }
    ];
    home-manager.users.charlotte = { ... }: {
      programs.qutebrowser = {
        enable = true;
        loadAutoconfig = true;
        aliases = {
          save-to-zotero = "jseval --quiet var d=document,s=d.createElement('script');s.src='https://www.zotero.org/bookmarklet/loader.js';(d.body?d.body:d.documentElement).appendChild(s);void(0);";
          open-in-temp = "spawn qutebrowser --temp-basedir -s downloads.location.directory /home/charlotte/downloads {url}";
          hint-open-in-temp = "hint links spawn qutebrowser --temp-basedir -s downloads.location.directory /home/charlotte/downloads {hint-url}";
        };
        keyBindings = {
          normal = {
            ",m" = "hint links spawn umpv {hint-url}";
            ",M" = "hint --rapid links spawn umpv {hint-url}";
            ",p" = "spawn --userscript qute-pass";

            # Toggle all by opening in temporary browser
            "tt" = "open-in-temp";
            "tT" = "hint-open-in-temp";

            # Cookie toggle (all removed from defaults)
            "tCH" = "config-cycle -p -u *://*.{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
            "tCh" = "config-cycle -p -u *://{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
            "tCu" = "config-cycle -p -u {url} content.cookies.accept no-3rdparty never ;; reload";
            "tcH" = "config-cycle -p -t -u *://*.{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
            "tch" = "config-cycle -p -t -u *://{url:host}/* content.cookies.accept no-3rdparty never ;; reload";
            "tcu" = "config-cycle -p -t -u {url} content.cookies.accept no-3rdparty never ;; reload";
          };
        };
        searchEngines = {
          DEFAULT = "https://html.duckduckgo.com/html?q={}";
        };
        settings = {
          auto_save.session = true;
          confirm_quit = [ "downloads" ];
          content = {
            autoplay = false;
            cookies.accept = "never";
            default_encoding = "utf-8";
            dns_prefetch = false;
            fullscreen.window = true;
            javascript.enabled = false;
            geolocation = false;
            pdfjs = true;
          };
          editor.command = [ "${config.chvp.base.emacs.package}/bin/emacsclient" "-c" "+{line}:{column}" "{file}" ];
          url.default_page = "about:blank";
          window.hide_decoration = true;
        };
      };
    };
  };
}
