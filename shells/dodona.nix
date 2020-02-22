let
  pkgs = import <nixpkgs> {};
  nixpkgs-master = import <nixpkgs-master> {};
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
  nodePackages = import ../packages/node/default.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      ruby
      yarn
      nodejs-12_x
      libmysqlclient
      zlib
      (pkgs.writeScriptBin "start-db" ''
        #!${pkgs.zsh}/bin/zsh

        trap "docker stop dodona-db" 0
        docker run --name dodona-db -p 3306:3306 --rm -v dodona-db-data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest &

        child=$!
        wait $child
      '')
      (nixpkgs-master.neovim.override {
        configure = {
          customRC = baseVimConfig.customRC + ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:deoplete#enable_at_startup = 1

            let g:LanguageClient_serverCommands = {
            \ 'ruby': ['${solargraph}/bin/solargraph', 'stdio'],
            \ 'javascript': ['${nodePackages.javascript-typescript-langserver}/bin/javascript-typescript-stdio'],
            \ 'typescript': ['${nodePackages.typescript-language-server}/bin/typescript-language-server', '--stdio'],
            \ }
          '';
          vam.knownPlugins = baseVimConfig.vam.knownPlugins;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [
            {
              names = [
                "deoplete-nvim"
                "LanguageClient-neovim"
                "vim-ruby"
                "yats-vim"
              ];
            }
          ];
        };
      })
    ];
    shellHook = ''
      export DATABASE_URL="mysql2://root:dodona@127.0.0.1:3306/dodona"
    '';
  }
