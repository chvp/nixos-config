self: super: {
  ssh = self.symlinkJoin {
    name = "openssh";
    paths = [
      (
        self.writeScriptBin "ssh" ''
          #!${self.zsh}/bin/zsh

          export TERM=xterm-256color
          ${super.openssh}/bin/ssh $@
        ''
      )
      super.openssh
    ];
  };
}
