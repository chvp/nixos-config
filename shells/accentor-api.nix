let
  pkgs = import <nixpkgs> {};
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ffmpeg
    postgresql
    ruby_2_7
    taglib
    zlib
    (
      neovim.override {
        configure = {
          customRC = baseVimConfig.customRC + ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:LanguageClient_serverCommands = {
            \ 'ruby': ['${solargraph}/bin/solargraph', 'stdio'],
            \ }
          '';
          vam.knownPlugins = baseVimConfig.vam.knownPlugins;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [
            {
              names = [
                "LanguageClient-neovim"
                "vim-ruby"
              ];
            }
          ];
        };
      }
    )
    (
      pkgs.writeScriptBin "start-db" ''
        #!/${pkgs.zsh}/bin/zsh

        trap "docker stop accentor-db" 0
        docker run --name accentor-db -p 5432:5432 --rm -v accentor-db-data:/var/lib/postgresql/data -e POSTGRES_PASSWORD=accentor postgres:latest &

        child=$!
        wait $child
      ''
    )
  ];
  shellHook = ''
    export DATABASE_URL="postgres://postgres:accentor@127.0.0.1:5432/accentor"
    export GEM_HOME="$PWD/vendor/rubygems/$(ruby -e 'puts RUBY_VERSION')"
    export PATH="$GEM_HOME/bin:$PATH"
  '';
}
