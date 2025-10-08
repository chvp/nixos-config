{ pkgs, inputs, ... }: pkgs.devshell.mkShell {
  name = "Papyros code editor";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  devshell = {
    motd = "";
    startup = {
      # Hack to make sure Rubymine doesn't use an ephemeral path from the nix store
      "link-devshell-dir".text = ''
        ln -snf $DEVSHELL_DIR $PRJ_DATA_DIR/devshell
      '';
    };
  };
  packages = with pkgs; [
    (python3.withPackages (ps: [ ps.pip ]))
    nodejs
    playwright-driver.browsers
    yarn
  ];
  env = [
    { name = "PLAYWRIGHT_BROWSERS_PATH"; value = pkgs.playwright-driver.browsers; }
    { name = "PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS"; value = "true"; }
  ];
  language.ruby = {
    package = pkgs.ruby_3_3;
    nativeDeps = [ ];
  };
}
