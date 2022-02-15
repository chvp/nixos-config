{ config, lib, pkgs, ... }:

let
  ff2mpv-host = pkgs.stdenv.mkDerivation rec {
    pname = "ff2mpv";
    version = "4.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "woodruffw";
      repo = "ff2mpv";
      rev = "v${version}";
      sha256 = "sxUp/JlmnYW2sPDpIO2/q40cVJBVDveJvbQMT70yjP4=";
    };
    buildInputs = [ pkgs.python3 ];
    buildPhase = ''
      sed -i "s#/home/william/scripts/ff2mpv#$out/bin/ff2mpv.py#" ff2mpv.json
      sed -i 's#"mpv"#"${pkgs.mpv}/bin/umpv"#' ff2mpv.py
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp ff2mpv.py $out/bin
      mkdir -p $out/lib/mozilla/native-messaging-hosts
      cp ff2mpv.json $out/lib/mozilla/native-messaging-hosts
    '';
  };
  ffPackage = pkgs.firefox.override {
    extraNativeMessagingHosts = [ ff2mpv-host ];
    pkcs11Modules = [ pkgs.eid-mw ];
    forceWayland = true;
    extraPolicies = {
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisableFirefoxAccounts = true;
      FirefoxHome = { Pocket = false; Snippets = false; };
      OfferToSaveLogins = false;
      UserMessaging = { SkipOnboarding = true; ExtensionRecommendations = false; };
    };
  };
  belgium-eid = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon rec {
    pname = "belgium-eid";
    version = "1.0.32";
    addonId = "belgiumeid@eid.belgium.be";
    url = "https://addons.mozilla.org/firefox/downloads/file/3736679/eid_belgie-${version}-fx.xpi";
    sha256 = "t2zbE58IuHeAlM91lNX4rbiWLzt5sQq350b1PRdSY7w=";
    meta = with lib; {
      homepage = "https://eid.belgium.be/";
      description = "Use the Belgian electronic identity card (eID) in Firefox";
      license = licenses.lgpl3;
      platforms = platforms.all;
    };
  };
  better-tweetdeck = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon rec {
    pname = "better-tweetdeck";
    version = "4.7.3";
    addonId = "BetterTweetDeck@erambert.me";
    url = "https://addons.mozilla.org/firefox/downloads/file/3901721/better_tweetdeck-${version}-fx.xpi";
    sha256 = "W07qVsK1mznE0sOiFtkyrRCqx+7D3732RA8LiaST6k8=";
    meta = with lib; {
      license = licenses.mit;
      platforms = platforms.all;
    };
  };
  zotero-connector = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon rec {
    pname = "zotero-connector";
    version = "5.0.92";
    addonId = "zotero@chnm.gmu.edu";
    url = "https://download.zotero.org/connector/firefox/release/Zotero_Connector-${version}.xpi";
    sha256 = "DfaDjjgJiSGJ0q9ScStAVRN3IcH8HY30K7IssuHZi2A=";
    meta = with lib; {
      homepage = "https://www.zotero.org";
      description = "Save references to Zotero from your web browser";
      license = licenses.agpl3;
      platforms = platforms.all;
    };
  };
in
{
  options.chvp.graphical.firefox = {
    enable = lib.mkEnableOption "firefox";
    package = lib.mkOption {
      description = "Final used firefox package";
      default = ffPackage;
      readOnly = true;
    };
  };

  config = lib.mkIf config.chvp.graphical.firefox.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".mozilla"; type = "data"; }
      { path = ".cache/mozilla"; type = "cache"; }
    ];
    home-manager.users.charlotte = { ... }: {
      programs = {
        browserpass = {
          enable = true;
          browsers = [ "firefox" ];
        };
        firefox = {
          enable = true;
          package = ffPackage;
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            belgium-eid
            better-tweetdeck
            browserpass
            decentraleyes
            ff2mpv
            https-everywhere
            ublock-origin
            umatrix
            zotero-connector
          ];
          profiles.default.settings = {
            "browser.aboutConfig.showWarning" = false;
            "browser.contentblocking.category" = "custom";
            "browser.download.dir" = "/home/charlotte/downloads";
            "browser.newtabpage.enabled" = false;
            "browser.safebrowsing.malware.enabled" = false;
            "browser.safebrowsing.phishing.enabled" = false;
            "browser.shell.checkDefaultBrowser" = false;
            "browser.startup.homepage" = "about:blank";
            "browser.startup.page" = 3;
            "dom.security.https_only_mode_pbm" = true;
            "network.cookie.cookieBehavior" = 1;
            "privacy.annotate_channels.strict_list.enabled" = true;
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "security.identityblock.show_extended_validation" = true;
          };
        };
      };
    };
  };
}
