{ ... }:

{
  services.skhd = {
    enable = true;
    skhdConfig = ''
      cmd - return : open -na /Applications/iTerm.app
      cmd + shift - return : open -na ~/Application/Emacs.app
    '';
  };
}
