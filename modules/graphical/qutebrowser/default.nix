{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.qutebrowser.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.qutebrowser.enable {
    home-manager.users.charlotte = { ... }: {
      programs.qutebrowser = {
        enable = true;
        loadAutoconfig = true;
        aliases = {
          save-to-zotero = "jseval --quiet var d=document,s=d.createElement('script');s.src='https://www.zotero.org/bookmarklet/loader.js';(d.body?d.body:d.documentElement).appendChild(s);void(0);";
        };
        keyBindings = {
          normal = {
            ",m" = "hint links spawn umpv {hint-url}";
            ",M" = "hint --rapid links spawn umpv {hint-url}";
            ",p" = "spawn --userscript qute-pass";
          };
        };
        settings = {
          auto_save.session = true;
          confirm_quit = [ "downloads" ];
          content = {
            autoplay = false;
            cookies.accept = "no-3rdparty";
            default_encoding = "utf-8";
            dns_prefetch = false;
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
