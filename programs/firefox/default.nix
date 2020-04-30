{ pkgs }:

(pkgs.firefox.override {
  extraNativeMessagingHosts = [
    (pkgs.passff-host.override { pass = (import ../pass/default.nix { inherit pkgs; }); })
  ];
})
