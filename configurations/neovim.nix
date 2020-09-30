{ pkgs, ... }:
let
  customPlugins = {
    snow-color-theme = pkgs.vimUtils.buildVimPlugin {
      name = "snow";
      src = pkgs.fetchFromGitHub {
        owner = "nightsense";
        repo = "snow";
        rev = "f9800e987e404efed4748fe8098e04ddbec9770b";
        sha256 = "099fky1bpppac5785bhx1jc26gfpm8n837p8487j1rf1lwq83q33";
      };
    };
  };
  jdtls = import ../packages/jdtls/default.nix { inherit pkgs; stdenv = pkgs.stdenv; };
  kotlinls = import ../packages/kotlin-language-server/default.nix { inherit pkgs; };
in
{
  custom.zfs.homeLinks = [
    { path = ".local/share/nvim"; type = "cache"; }
    { path = ".cache/nvim"; type = "cache"; }
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.sessionVariables = { EDITOR = "nvim"; };
    programs.neovim = {
      enable = true;
      extraConfig = ''
        set autoread
        "" Theming

        set termguicolors
        set background=light
        colorscheme snow

        "" General settings

        " Undo over sessions
        set undofile
        set undodir=~/.cache/nvim/undo

        " Automatically save sessions on exit and load them on start
        function! MakeSession()
          let b:sessiondir = $HOME . "/.local/share/nvim/sessions" . getcwd()
          if (filewritable(b:sessiondir) != 2)
            exe 'silent !mkdir -p ' b:sessiondir
            redraw!
          endif
          let b:filename = b:sessiondir . '/session.vim'
          exe "mksession! " . b:filename
        endfunction

        function! LoadSession()
          let b:sessiondir = $HOME . "/.local/share/nvim/sessions" . getcwd()
          let b:sessionfile = b:sessiondir . "/session.vim"
          if (filereadable(b:sessionfile))
            exe 'source ' b:sessionfile
          else
            echo "No session loaded."
          endif
        endfunction
        if(argc() == 0)
          au VimEnter * nested :call LoadSession()
          au VimLeave * :call MakeSession()
        endif

        "" Filetype configuration

        " Base settings for all files

        syntax enable
        set number
        set showcmd
        set scrolloff=8
        set expandtab
        set tabstop=4
        set shiftwidth=4
        set linebreak

        set list
        set listchars=tab:·\ ,trail:·
        set inccommand=split
        set clipboard=unnamedplus

        filetype plugin indent on

        "" Fuzzy search in vim

        function! s:completedFiles(winid, filename, ...) abort
          bdelete!
          call win_gotoid(a:winid)
          if filereadable(a:filename)
            let lines = readfile(a:filename)
            if !empty(lines)
              exe ':e ' . lines[0]
            endif
            call delete(a:filename)
          endif
        endfunction

        function! FzyFiles()
          let file = tempname()
          let winid = win_getid()
          let cmd = split(&shell) + split(&shellcmdflag) + ["${pkgs.ripgrep.out}/bin/rg --files --hidden -g '!/.git' --smart-case | ${pkgs.fzy.out}/bin/fzy > " . file]
          let F = function('s:completedFiles', [winid, file])
          botright 10 new
          call termopen(cmd, {'on_exit': F})
          startinsert
        endfunction

        function! s:completedGrep(winid, filename, ...) abort
          bdelete!
          call win_gotoid(a:winid)
          if filereadable(a:filename)
            let lines = readfile(a:filename)
            if !empty(lines)
              let list = split(lines[0], ':')
              let file = list[0]
              let line = list[1]
              exe ':e ' . file
              exe line
            endif
            call delete(a:filename)
          endif
        endfunction

        function! FzyGrep()
          let file = tempname()
          let winid = win_getid()
          let cmd = split(&shell) + split(&shellcmdflag) + ["${pkgs.ripgrep.out}/bin/rg --vimgrep --hidden -g '!/.git' '^' | ${pkgs.fzy.out}/bin/fzy > " . file]
          let F = function('s:completedGrep', [winid, file])
          botright 10 new
          call termopen(cmd, {'on_exit': F})
          startinsert
        endfunction

        nnoremap <C-f> :call FzyFiles()<CR>
        nnoremap <C-g> :call FzyGrep()<CR>
      '';
      plugins = with pkgs.vimPlugins // customPlugins; [
        {
          plugin = ale;
          config = ''
            let g:ale_fixers = {
            \  '*': ['remove_trailing_lines', 'trim_whitespace'],
            \  'javascript': ['eslint', 'remove_trailing_lines', 'trim_whitespace'],
            \  'ledger': ['trim_whitespace'],
            \  'nix': ['nixpkgs-fmt', 'remove_trailing_lines', 'trim_whitespace'],
            \  'ruby': ['rubocop', 'remove_trailing_lines', 'trim_whitespace'],
            \  'typescript': ['eslint', 'remove_trailing_lines', 'trim_whitespace'],
            \  'vue': ['prettier', 'remove_trailing_lines', 'trim_whitespace'],
            \}
            let g:ale_fix_on_save = 1
          '';
        }
        auto-pairs
        {
          plugin = deoplete-nvim;
          config = ''
            let g:deoplete#enable_at_startup = 1
            set completeopt+=noselect
            au VimEnter * call deoplete#custom#option('omni_patterns', {
            \ 'ledger': ['[a-zA-Z][a-zA-Z: ]*'],
            \})
          '';
        }
        editorconfig-vim
        kotlin-vim
        {
          plugin = LanguageClient-neovim;
          config = ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:LanguageClient_serverCommands = {
            \ 'java': ['${jdtls}/bin/jdtls'],
            \ 'javascript': ['${pkgs.nodePackages.javascript-typescript-langserver}/bin/javascript-typescript-stdio'],
            \ 'kotlin': ['${kotlinls}/bin/kotlin-language-server'],
            \ 'ruby': ['${pkgs.solargraph}/bin/solargraph', 'stdio'],
            \ 'typescript': ['${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server', '--stdio'],
            \ 'vue': ['${pkgs.nodePackages.vue-language-server}/bin/vls'],
            \ }

            function LC_maps()
              if has_key(g:LanguageClient_serverCommands, &filetype)
                nnoremap <buffer> <silent> K :call LanguageClient#textDocument_hover()<cr>
                nnoremap <buffer> <silent> gd :call LanguageClient#textDocument_definition()<CR>
                nnoremap <buffer> <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
              endif
            endfunction

            autocmd FileType * call LC_maps()
          '';
        }
        snow-color-theme
        vim-ledger
        vim-nix
        vim-ruby
        vim-vue
        yats-vim
      ];
    };
  };
}
