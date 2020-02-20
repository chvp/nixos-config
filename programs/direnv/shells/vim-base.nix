{ pkgs, ... }:

{
  customRC = ''
    set autoread
    "" Theming

    set background=light

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
    endif
    au VimLeave * :call MakeSession()

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

    "" Plugin configuration

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
  vam.knownPlugins = pkgs.vimPlugins;
  vam.pluginDictionaries = [ { name = "vim-nix"; } ];
}
