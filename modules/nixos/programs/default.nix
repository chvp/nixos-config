{ pkgs, ... }:

{
  imports = [
    ./calibre
    ./eid
    ./element
    ./teams
    ./torrents
  ];
}
