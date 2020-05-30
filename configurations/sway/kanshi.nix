{ ... }:

{
  home-manager.users.charlotte = { ... }: {
    xdg.configFile."kanshi/config".text = ''
      {
        output "Unknown 0x2036 0x00000000" position 0,0 mode 2560x1440 scale 1.0
      }
      {
        output "Unknown 0x2036 0x00000000" position 0,0 mode 2560x1440 scale 1.0
        output "Dell Inc. DELL U2718Q FN84K01T095L" position 2560,0 mode 3840x2160 scale 1.25
      }
      {
        output "Chimei Innolux Corporation 0x14D3 0x00000000" position 0,0 mode 1920x1080 scale 1
      }
      {
        output "Chimei Innolux Corporation 0x14D3 0x00000000" position 0,0 mode 1920x1080 scale 1
        output "Dell Inc. DELL U2718Q FN84K83Q1KHL" position 1920,0 mode 3840x2160 scale 1.25
      }
    '';
  };
}
