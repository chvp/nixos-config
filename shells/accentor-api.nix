let
  pkgs = import <nixpkgs> { };
  baseVimConfig = import ../configurations/neovim/base.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ffmpeg
    postgresql
    ruby_2_7
    taglib
    zlib
    (
      pkgs.writeScriptBin "start-db" ''
        #!/${pkgs.zsh}/bin/zsh

        trap "systemd-run --user --no-block docker stop accentor-db" 0
        docker run -d --name accentor-db -p 5432:5432 --rm -v accentor-db-data:/var/lib/postgresql/data -e POSTGRES_PASSWORD=accentor postgres:latest

        while [ 1 -eq 1 ]
        do
          sleep 1000
        done
      ''
    )
  ];
  shellHook = ''
    export DATABASE_URL="postgres://postgres:accentor@127.0.0.1:5432/accentor"
    export GEM_HOME="$PWD/vendor/rubygems/$(ruby -e 'puts RUBY_VERSION')"
    export PATH="$GEM_HOME/bin:$PATH"
  '';
}
