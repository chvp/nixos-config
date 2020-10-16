let
  pkgs = import <nixpkgs> { config.android_sdk.accept_license = true; };
  buildToolsVersion = "29.0.2";
  composed = pkgs.androidenv.composeAndroidPackages {
    toolsVersion = "26.1.1";
    platformToolsVersion = "29.0.6";
    buildToolsVersions = [ buildToolsVersion ];
    # includeEmulator = false;
    # emulatorVersion = "27.2.0";
    platformVersions = [ "29" ];
    includeSources = true;
    includeDocs = true;
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
  gradle-fhs-nix = pkgs.writeText "gradle-fhs.nix" ''
    { run }:
    let
      pkgs = import <nixpkgs> {};
    in
      (pkgs.buildFHSUserEnv {
        name = "android-sdk-env";
        targetPkgs = pkgs: (with pkgs; [ glibc ]);
        profile = '''
          export ANDROID_SDK_ROOT="${composed.androidsdk}/libexec/android-sdk/"
        ''';
        runScript = "bash -c '''''${run}'''";
      }).env
  '';
  gradle-run-script = pkgs.writeShellScriptBin "gradle" ''
    REPO_ROOT="$(git rev-parse --show-toplevel)"
    nix-shell --argstr run "\"$REPO_ROOT/gradlew $@\"" "${gradle-fhs-nix}"
  '';
  sign-release = pkgs.writeShellScriptBin "sign-release" ''
    BUILD_TOOLS_PATH="${composed.androidsdk}/libexec/android-sdk/build-tools/${buildToolsVersion}"
    REPO_ROOT="$(git rev-parse --show-toplevel)"
    APK_DIR="$REPO_ROOT/app/build/outputs/apk/release"

    rm "$APK_DIR/"*
    ${gradle-run-script}/bin/gradle assembleRelease
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
  ];
  shellHook = ''
    export ANDROID_SDK_ROOT="${composed.androidsdk}/libexec/android-sdk/"
  '';
}
