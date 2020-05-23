{ pkgs, ... }:

{
  services.pcscd = {
    enable = true;
    plugins = [ pkgs.ccid ];
  };

  environment.systemPackages = with pkgs; [ eid-mw ];
}
