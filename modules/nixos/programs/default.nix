{ pkgs, ... }:

{
  imports = [
    ./calibre
    ./eid
    ./element
    ./obs
    ./torrents
  ];
}
