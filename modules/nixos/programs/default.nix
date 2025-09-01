{ pkgs, ... }:

{
  imports = [
    ./calibre
    ./eid
    ./element
    ./slack
    ./teams
    ./torrents
  ];
}
