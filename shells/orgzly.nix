{ inputs, system, ... }: let
    pkgs = import inputs.nixpkgs { inherit system; config = { android_sdk.accept_license = true; allowUnfree = true; }; overlays = [ inputs.devshell.overlays.default ]; };
    buildToolsVersion = "30.0.3";
    composed = pkgs.androidenv.composeAndroidPackages {
        buildToolsVersions = [ buildToolsVersion ];
        platformVersions = [ "32" ];
    };
    fhsEnv = pkgs.buildFHSUserEnv {
        name = "android-sdk-env";
        targetPkgs = pkgs: (with pkgs; [ glibc ]);
        profile = ''
        export ANDROID_SDK_ROOT="${composed.androidsdk}/libexec/android-sdk/"
        '';
    };
in
  pkgs.devshell.mkShell {
  name = "Orgzly";
  packages = [ pkgs.jdk17 pkgs.kotlin-language-server pkgs.nixpkgs-fmt ];
  env = [
    { name = "ANDROID_SDK_ROOT"; eval = "${composed.androidsdk}/libexec/android-sdk/"; }
    { name = "BUILD_TOOLS_PATH"; eval = "$ANDROID_SDK_ROOT/build-tools/${buildToolsVersion}"; }
    { name = "APK_DIR"; eval = "$PRJ_ROOT/app/build/outputs/apk/premium/release"; }
  ];
  commands = [
    {
      name = "gradle";
      category = "tools";
      help = "Working gradle invocation";
      command = "${fhsEnv}/bin/android-sdk-env \"$PRJ_ROOT/gradlew\" $@";
    }
    {
      name = "install-debug-signed-release";
      category = "tools";
      help = "Install a debug signed release APK";
      command = ''
                  rm -f "$APK_DIR/"*
                  gradle assembleRelease
                  "$BUILD_TOOLS_PATH/zipalign" -v -p 4 "$APK_DIR/app-premium-release-unsigned.apk" "$APK_DIR/app-release-unsigned-aligned.apk"
                  echo android | "$BUILD_TOOLS_PATH/apksigner" sign --ks "$HOME/.android/debug.keystore" --out "$APK_DIR/app-release.apk" "$APK_DIR/app-release-unsigned-aligned.apk"
                  adb install -r "$APK_DIR/app-release.apk"
                '';
    }
  ];
}
