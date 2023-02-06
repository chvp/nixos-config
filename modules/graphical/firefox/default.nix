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
  zotero-connector = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon rec {
    pname = "zotero-connector";
    version = "5.0.102";
    addonId = "zotero@chnm.gmu.edu";
    url = "https://download.zotero.org/connector/firefox/release/Zotero_Connector-${version}.xpi";
    sha256 = "ZxUypalKfax0Jo617hEAdeNa7UM4WGv4tKafMaWLXkI=";
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
          profiles.default = {
            extensions = with pkgs.nur.repos.rycee.firefox-addons; [
              belgium-eid
              browserpass
              decentraleyes
              ff2mpv
              ublock-origin
              umatrix
              zotero-connector
            ];
            settings = {
              "browser.aboutConfig.showWarning" = false;
              "browser.contentblocking.category" = "custom";
              "browser.download.dir" = "/home/charlotte/downloads";
              "browser.newtabpage.enabled" = false;
              "browser.safebrowsing.malware.enabled" = false;
              "browser.safebrowsing.phishing.enabled" = false;
              "browser.shell.checkDefaultBrowser" = false;
              "browser.startup.homepage" = "about:blank";
              "browser.startup.page" = 3;
              "dom.security.https_only_mode" = true;
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
  };
}
