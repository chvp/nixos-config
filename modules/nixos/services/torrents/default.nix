{ config, lib, pkgs, ... }:

{
  options.chvp.services.torrents = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf config.chvp.services.torrents.enable {
    chvp.services.nginx.hosts = [{ fqdn = "transmission.vanpetegem.me"; basicProxy = "http://localhost:9091"; }];

    nixpkgs.overlays = [
      (self: super: {
        transmission_4 = super.transmission_4.overrideAttrs (old: {
          version = "4.0.5";
          src = pkgs.fetchFromGitHub {
            owner = "transmission";
            repo = "transmission";
            rev = "4.0.5";
            hash = "sha256-gd1LGAhMuSyC/19wxkoE2mqVozjGPfupIPGojKY0Hn4=";
            fetchSubmodules = true;
          };

          patches = [
            (pkgs.fetchpatch2 {
              url = "https://github.com/transmission/transmission/commit/febfe49ca3ecab1a7142ecb34012c1f0b2bcdee8.patch?full_index=1";
              hash = "sha256-Ge0+AXf/ilfMieGBAdvvImY7JOb0gGIdeKprC37AROs=";
              excludes = [
                # The submodule that we don't use (we use our miniupnp)
                "third-party/miniupnp"
                # Hunk fails for this one, but we don't care because we don't rely upon
                # xcode definitions even for the Darwin build.
                "Transmission.xcodeproj/project.pbxproj"
              ];
            })
          ];

          postPatch = ''
            # Clean third-party libraries to ensure system ones are used.
            # Excluding gtest since it is hardcoded to vendored version. The rest of the listed libraries are not packaged.
            pushd third-party
            for f in *; do
                if [[ ! $f =~ googletest|wildmat|fast_float|wide-integer|jsonsl ]]; then
                    rm -r "$f"
                fi
            done
            popd
            rm \
              cmake/FindFmt.cmake \
              cmake/FindUtfCpp.cmake
            # Upstream uses different config file name.
            substituteInPlace CMakeLists.txt --replace 'find_package(UtfCpp)' 'find_package(utf8cpp)'

            # Use gettext even on Darwin
            substituteInPlace libtransmission/utils.h \
              --replace-fail '#if defined(HAVE_GETTEXT) && !defined(__APPLE__)' '#if defined(HAVE_GETTEXT)'
          '';

          postInstall = builtins.replaceStrings [ "/icons/hicolor_apps_scalable_transmission.svg" ] [ "/qt/icons/transmission.svg" ] old.postInstall;
        });
      })
    ];

    services.transmission = {
      enable = true;
      package = pkgs.transmission_4;
      user = "charlotte";
      group = "users";
      home = "/var/lib/transmission";
      openRPCPort = false;
      openPeerPorts = true;
      credentialsFile = config.age.secrets."files/programs/transmission/config.json".path;
      settings = {
        umask = 18;
        download-dir = "/srv/data";
        incomplete-dir = "/srv/data/.incomplete";
        rpc-authentication-required = true;
        rpc-bind-address = "0.0.0.0";
        rpc-enabled = true;
        rpc-host-whitelist-enabled = false;
        rpc-whitelist-enabled = false;
        speed-limit-down = 51200;
        speed-limit-down-enabled = true;
      };
    };
    systemd.services.transmission.serviceConfig.TimeoutStartSec = 60 * 10;
    age.secrets."files/programs/transmission/config.json" = {
      file = ../../../../secrets/files/programs/transmission/config.json.age;
      owner = "charlotte";
    };
  };
}
