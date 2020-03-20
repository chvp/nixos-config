let
  pkgs = import <nixpkgs> { config.android_sdk.accept_license = true; };
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
  jdtls = import ../packages/jdtls/default.nix { inherit pkgs; stdenv = pkgs.stdenv; };
  kotlinls = import ../packages/kotlin-language-server/default.nix { inherit pkgs; };
  composed = pkgs.androidenv.composeAndroidPackages {
    toolsVersion = "26.1.1";
    platformToolsVersion = "28.0.1";
    buildToolsVersions = [ "28.0.3" ];
    # includeEmulator = false;
    # emulatorVersion = "27.2.0";
    platformVersions = [ "29" ];
    includeSources = false;
    includeDocs = false;
    # includeSystemImages = false;
    # systemImageTypes = [ "default" ];
    # abiVersions = [ "armeabi-v7a" ];
    # lldbVersions = [ "2.0.2558144" ];
    # cmakeVersions = [ "3.6.4111459" ];
    # includeNDK = false;
    # ndkVersion = "16.1.4479499";
    # useGoogleAPIs = false;
    # useGoogleTVAddOns = false;
    # includeExtras = [ "extras;google;gcm" ];
  };

  customPlugins.kotlin-vim = pkgs.vimUtils.buildVimPlugin {
    name = "kotlin-vim";
    src = pkgs.fetchFromGitHub {
      owner = "udalov";
      repo = "kotlin-vim";
      rev = "b9fa728701a0aa0b9a2ffe92f10880348fc27a8f";
      sha256 = "1yqzxabhpc4jbdlzhsysp0vi1ayqg0vnpysvx4ynd9961q2fk3sz";
    };
  };

  gradle-fhs-nix = pkgs.writeText "gradle-fhs.nix" ''
    { run ? "bash" }:
    let
      pkgs = import <nixpkgs> {};
    in
      (pkgs.buildFHSUserEnv {
        name = "android-sdk-env";
        targetPkgs = pkgs: (with pkgs;
          [
            glibc
          ]);
        profile = '''
          export ANDROID_SDK_ROOT="${composed.androidsdk}/libexec/android-sdk/"
        ''';
        runScript = "''${run}";
      }).env
    '';

    gradle-run-script = pkgs.writeScriptBin "gradle" ''
      #!${pkgs.bash}/bin/bash

      nix-shell --argstr run "./gradlew $@" "${gradle-fhs-nix}"
    '';

    sign-release = pkgs.writeScriptBin "sign-release" ''
      #!${pkgs.bash}/bin/bash

      BUILD_TOOLS_PATH="${composed.androidsdk}/libexec/android-sdk/build-tools/28.0.3"
      REPO_ROOT="$(git rev-parse --show-toplevel)"
      APK_DIR="$REPO_ROOT/app/build/outputs/apk/release"

      rm "$APK_DIR/app-release-unsigned-aligned.apk"
      rm "$APK_DIR/app-release.apk"
      "$BUILD_TOOLS_PATH/zipalign" -v -p 4 "$APK_DIR/app-release-unsigned.apk" "$APK_DIR/app-release-unsigned-aligned.apk"

      "$BUILD_TOOLS_PATH/apksigner" sign --ks "$REPO_ROOT/keystore.jks" --out "$APK_DIR/app-release.apk" "$APK_DIR/app-release-unsigned-aligned.apk"
      "$BUILD_TOOLS_PATH/apksigner" verify "$APK_DIR/app-release.apk"
    '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    gradle-run-script
    sign-release
    jdk11
    jdtls
    (
      neovim.override {
        configure = {
          customRC = baseVimConfig.customRC + ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:LanguageClient_serverCommands = {
            \ 'java': ['${jdtls}/bin/jdtls'],
            \ 'kotlin': ['${kotlinls}/bin/kotlin-language-server']
            \ }
          '';
          vam.knownPlugins = baseVimConfig.vam.knownPlugins // customPlugins;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [
            {
              names = [
                "kotlin-vim"
                "LanguageClient-neovim"
              ];
            }
          ];
        };
      }
    )
  ];
  shellHook = ''
    export ANDROID_SDK_ROOT="${composed.androidsdk}/libexec/android-sdk/"
  '';
}
