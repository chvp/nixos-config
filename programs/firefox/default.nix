{ pkgs }:

(pkgs.firefox.override {
  extraNativeMessagingHosts = [
    (pkgs.passff-host.override { pass = (import ../../programs/pass/default.nix { inherit pkgs; }); })
  ];
})
