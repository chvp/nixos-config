{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Dodona Docs";
  env = [{ name = "PUPPETEER_EXECUTABLE_PATH"; eval = "${pkgs.ungoogled-chromium}/bin/chromium"; }];
  packages = with pkgs; [ nodejs yarn ];
}
