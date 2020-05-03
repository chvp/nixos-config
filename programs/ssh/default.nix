{ pkgs }:

pkgs.symlinkJoin {
  name = "openssh";
  paths = [
    (
      pkgs.writeScriptBin "ssh" ''
        #!${pkgs.zsh}/bin/zsh

        export TERM=xterm-256color
        ${pkgs.openssh}/bin/ssh $@
      ''
    )
    pkgs.openssh
  ];
}
