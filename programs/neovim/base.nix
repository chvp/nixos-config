{ pkgs, ... }:

let
  customPlugins.snow-color-theme = pkgs.vimUtils.buildVimPlugin {
    name = "snow";
    src = pkgs.fetchFromGitHub {
      owner = "nightsense";
      repo = "snow";
      rev = "f9800e987e404efed4748fe8098e04ddbec9770b";
      sha256 = "099fky1bpppac5785bhx1jc26gfpm8n837p8487j1rf1lwq83q33";
    };
  };

  customPlugins.deoplete-old = pkgs.vimUtils.buildVimPluginFrom2Nix {
    name = "deoplete-old";
    src = pkgs.fetchFromGitHub {
      owner = "Shougo";
      repo = "deoplete.nvim";
      rev = "e897e0142759eb7ffbded565389243cab6a09a91";
      sha256 = "00qvpp7r7wnccfzfxq9xa4cyxzr25zy32mpxscnbixc7cv5y981x";
    };
  };
in
{
  customRC = ''
    set autoread
    "" Theming

    set background=light
    colorscheme snow

    "" General settings

    " Undo over sessions
    set undofile
    set undodir=~/.cache/nvimundo

    " Automatically save sessions on exit and load them on start
    function! MakeSession()
      let b:sessiondir = $HOME . "/.config/nvim/sessions" . getcwd()
      if (filewritable(b:sessiondir) != 2)
        exe 'silent !mkdir -p ' b:sessiondir
        redraw!
      endif
      let b:filename = b:sessiondir . '/session.vim'
      exe "mksession! " . b:filename
    endfunction

    function! LoadSession()
      let b:sessiondir = $HOME . "/.config/nvim/sessions" . getcwd()
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

    "" Plugin configuration
    let g:ale_fixers = { '*': ['remove_trailing_lines', 'trim_whitespace'] }
    let g:ale_fix_on_save = 1

    let g:deoplete#enable_at_startup = 1
  '';
  vam.knownPlugins = pkgs.vimPlugins // customPlugins;
  vam.pluginDictionaries = [
    {
      names = [
        "ale"
        "auto-pairs"
        "deoplete-old"
        "editorconfig-vim"
        "snow-color-theme"
        "vim-nix"
      ];
    }
  ];
}
