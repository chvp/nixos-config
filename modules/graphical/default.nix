{ config, lib, pkgs, ... }:

let
  ligature = pkgs.fetchFromGitHub {
    owner = "mickeynp";
    repo = "ligature.el";
    rev = "9357156a917a021a87b33ee391567a5d8e44794a";
    hash = "sha256-Bgb5wFyx0hMilpihxA8cTrRVw71EBOw2DczlM4lSNMs=";
  };
in
{
  imports = [
    ./firefox
    ./gnupg
    ./mail
    ./pass
    ./sound
    ./sway
    ./syncthing
    ./terminal
    ./theme
    ./xdg
  ];

  options.chvp.graphical.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.enable {
    users.users.charlotte.extraGroups = [ "input" "video" ];
    chvp = {
      base = {
        emacs.extraConfig = [
          ''
            ;; Ligatures in GUI mode
            (use-package ligature
              :load-path "${ligature}"
              :config
              (ligature-set-ligatures 't '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                           ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                           "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                           "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                           "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                           "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                           "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                           "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                           "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                           "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")
              )
              (global-ligature-mode 't)
            )
          ''
        ];
        nix.unfreePackages = [ "google-chrome" ];
      };
      graphical = {
        firefox.enable = lib.mkDefault true;
        gnupg = {
          enable = lib.mkDefault true;
          pinentryFlavor = "qt";
        };
        mail.enable = lib.mkDefault true;
        pass.enable = lib.mkDefault true;
        sound.enable = lib.mkDefault true;
        sway.enable = lib.mkDefault true;
        syncthing.enable = lib.mkDefault true;
        terminal.enable = lib.mkDefault true;
        theme.enable = lib.mkDefault true;
        xdg.enable = lib.mkDefault true;
      };
    };

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [
        google-chrome
        mpv
        okular
        ranger
        uni
        yt-dlp
      ];
    };
  };
}
