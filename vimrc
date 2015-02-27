" file: ~/.vimrc
" author: d4ryus - https://github.com/d4ryus/
" vim:ts=8:sw=8:foldmethod=marker:

" vundle {{{1

" settings {{{2

filetype off
set nocompatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" settings }}}2

Bundle 'gmarik/vundle'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-dispatch'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'dhruvasagar/vim-table-mode'
Bundle 'gregsexton/gitv'
Bundle 'godlygeek/tabular'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/NERDTree'
Bundle 'mtth/scratch.vim'
Bundle 'airblade/vim-gitgutter'
Bundle 'kien/ctrlp.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'AlxHnr/clear_colors'
Bundle 'andrwb/vim-lapis256'

if executable('ctags')
        Bundle 'vim-scripts/taglist.vim'
endif

if has("python")
        Bundle 'kovisoft/slimv'
endif

if has("python") || has("python3")
        Bundle 'SirVer/ultisnips'
        Bundle 'sjl/gundo.vim'
endif

" vundle }}}1
" plugin-settings {{{1

" NERDtree {{{2

let NERDTreeDirArrows=0

" NERDtree }}}2
" UltiSnips {{{2

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

" UltiSnips }}}2
" Eclim {{{2

let g:EclimLoggingDisabled=1    " disable Eclim logging
let g:EclimCompletionMethod='omnifunc'

" Eclim }}}2
" Supertab {{{2

let g:SuperTabDefaultCompletionType='context'

" Supertab }}}2
" Taglist {{{2

let Tlist_Use_Right_Window = 1
let Tlist_File_Fold_Auto_Close = 1

" Taglist }}}2
" Ag {{{2

if executable('ag')
        set grepprg=ag\ --nogroup\ --nocolor
endif

" Ag }}}2
" CtrlP {{{2

let g:ctrlp_extensions = ['tag']
let g:ctrlp_switch_buffer = 'E'
let g:ctrlp_custom_ignore = {
        \ 'dir':  '\v[\/]\.(git|hg|svn)$',
        \ 'file': '\v\.(o|a|so|class|jpg|jpeg|bmp|tar|jar|exe|dll)$',
        \ }

if executable('ag')
        let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
        let g:ctrlp_use_caching = 0
endif

" CtrlP }}}2
" Scratch {{{2

let g:scratch_insert_autohide = 0

" Scratch }}}2
" GitGutter {{{2

let g:gitgutter_enabled = 0

" GitGutter }}}2
" GitV {{{2

let g:Gitv_OpenPreviewOnLaunch = 1
let g:Gitv_DoNotMapCtrlKey = 1

" GitV }}}2
" Gundo {{{2

if has("python3")
        let g:gundo_prefer_python3 = 1
endif

" Gundo }}}2
" Slimv {{{2

let g:slimv_repl_split = 4

" Slimv }}}2

" plugin-settings }}}1
" macros {{{1

map :fts zt,,f{azfa{j
map :haw :0r ~/.vim/license/haw.txt<CR>

" macros }}}1
" setter {{{1

syntax on                       " enable syntax highlighting
set ruler                       " show the cursor position all the time
set showcmd                     " display incomplete commands
set incsearch                   " do incremental searching
set ignorecase                  " dont use case sensetive search
set nocompatible                " set noncompatible mode (vi vim)
set autoread                    " autoread file when changed from outside
set autoindent                  " always set autoindenting on
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set list                        " list all tabs and ending spaces
set listchars=tab:>·,nbsp:_,trail:·,precedes:«,extends:»,eol:¬,conceal:_
set linebreak                   " better wraping of lines
set breakindent                 " indent linebreaks
set showbreak=\ »»\             " show linebreaks if wrap is set
set expandtab                   " use spaces instead of tabs
set tabstop=8                   " amout of spaces per tab
set shiftwidth=8                " number of spaces used by autoindent
set cm=blowfish2                " use blowfish as encryption (X)
set history=82                  " keep 82 lines of command line history
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set encoding=UTF-8              " use UTF-8 as encoding
set modelines=40                " search first/last 40 lines for vim modeline options
set laststatus=2                " allways show statusline
set spelllang=en,de             " set spelling language to english and german
set directory=~/.vim/swap//     " directory where all swap files will be
set foldtext=NeatFoldText()     " set foldtext to function below
set completeopt=longest,menuone " dont select the first match
set splitright                  " open splits on the right side instead of left
set splitbelow                  " open splits blow instead of on top
set clipboard=unnamedplus       " paste from clipboard, yank to clipboard
set wildmenu                    " use wildmenu
set wildmode=longest:list,full  " dont change insertion
set tags+=.git/tags             " source git repository tags
set path+=/usr/local/include    " also search through /usr/local/include
set path+=./**                  " also add the current path

if has('persistent_undo')
    let dir = expand('$HOME/.vim/undo')
    call system('mkdir ' . dir)
    let &undodir = dir
    set undofile
endif

if executable('par')
    set formatprg=par
endif

" setter }}}1
" mappings {{{1

" noremap {{{2

noremap <Leader>t :TlistToggle<CR>
noremap <Leader>u :GundoToggle<CR>
noremap zv zMzv
noremap tab :Tabularize /
noremap go :Make<CR>

noremap cog :GitGutterToggle<CR>
noremap cop :RainbowParenthesesToggle<CR>

nnoremap <Up>    :res +1<CR>
nnoremap <Down>  :res -1<CR>
nnoremap <Left>  :vertical res -1<CR>
nnoremap <Right> :vertical res +1<CR>

" noremap }}}2
" inoremap {{{2

inoremap jk <Esc>

" inoremap }}}2
" vnoremap {{{2

vnoremap < <gv
vnoremap > >gv

" vnoremap }}}2
" cnoremap {{{2

cnoremap <expr> %% getcmdtype() == ':' ? expand('%h').'/' : '%%'

" cnoremap}}}2

" mappings }}}1
" abbrev {{{1

iabbrev date- <c-r>=strftime("%Y-%m-%d")<cr>
iabbrev file- <c-r>%<cr>

" abbrev }}}1
" appearance {{{1

" statusline {{{2

set statusline=%<[%F]\ [%{&ff}]\ [%M%Y%R%q%W]\ %{fugitive#statusline()}%=\ [pos:\ %l/%L:%c\ %p%%]

" statusline }}}2
" overlength {{{2

highlight OverLength ctermbg=233 guibg=#592929
match OverLength /\%81v.\+/

" overlength }}}2
" colorscheme {{{2

colorscheme vividchalk
" if &term =~ "xterm"                 ||
" \  &term =~ "urxvt"                 ||
" \  &term =~ "xterm-256color"        ||
" \  &term =~ "screen-256color"       ||
" \  &term =~ "rxvt-unicode-256color" ||
" \  has("gui_running")
"         colorscheme d4ryus_256
" else
"         colorscheme d4ryus_8
" endif

" colorscheme }}}2

" appearance }}}1
" functions {{{1

" NeatFoldText {{{2

function! NeatFoldText()
" got this function from http://dhruvasagar.com/tag/vim thanks alot :)
        let line             = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
        let lines_count      = v:foldend - v:foldstart + 1
        let lines_count_text = '| ' . printf("%9s", lines_count . ' lines') . ' |'
        "let foldchar         = matchstr(&fillchars, 'fold:\zs.')
        let foldchar         = '_'
        let foldtextstart    = strpart('+' . repeat(foldchar, v:foldlevel) . line, 0, (winwidth(0)*2)/3)
        let foldtextend      = lines_count_text . repeat(foldchar, 8)
        let foldtextlength   = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
        return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction

" NeatFoldText }}}2
" NumberFold {{{2

function! NumberFold()
        let h = matchstr(getline(v:lnum), '^\d\+')
        if empty(h)
                return "="
        else
                return ">" . len(h)
        endif
endfunction

" NumberFold }}}2
" PatchFold {{{2

function! PatchFold()
        let h = getline(v:lnum)
        if match(h, '^diff') >= 0
                return ">1"
        elseif match(h, '^\@\@') >= 0
                return ">2" . len(h)
        else
                return "="
        endif
endfunction

" PatchFold }}}2
" RemoveTrailingWhitespaces {{{2

function! RemoveTrailingWhitespaces()
        silent! %s/\s\+$//e
endfunction

" RemoveTrailingWhitespaces }}}2
" Bash {{{2

function! Bash()
        let prompt = '>'
        call search(prompt, 'b')
        let cmd = substitute(getline('.'), '^.*' . prompt, '', '')
        set paste
        call append(line('.'), ['', '', prompt])
        call append(line('.') + 1, systemlist(cmd))
        set nopaste
        call search('^' . prompt)
endfunction

" Bash }}}2
" Markdown_preview {{{2

function! Markdown_preview()
        if !executable('markdown')
                echo 'cannot find markdown executable'
                return
        endif
        if !executable('xdotool')
                echo 'cannot find xdotool executable'
                return
        endif
        if !executable('chromium')
                echo 'cannot find chromium executable'
                return
        endif

        let file_name = expand('%:t')
        let tmp_file_name = file_name . '.html'

        call system('markdown ' . file_name . ' > /tmp/' . tmp_file_name)

        let browser = system("xdotool search --name '" . tmp_file_name . " - Chromium'")
        sleep 300m

        let curr_win = system('xdotool getwindowfocus')
        if !browser
                call system('chromium /tmp/' . tmp_file_name)
        else
                call system('xdotool windowmap ' . browser)
                call system('xdotool windowactivate ' . browser)
        endif
        call system("xdotool key 'ctrl+r'")
        call system('xdotool windowactivate ' . curr_win)
endfunction

" Markdown_preview }}}2

" functions }}}1
" commands {{{1

command! WQ wq
command! Wq wq
command! W w
command! Q q

command! Codestyle call ApplyCodeStyle()
command! Rtw call RemoveTrailingWhitespaces()
command! Bash call Bash()

command! Jio JavaImportOrganize
command! Jdp JavaDocPreview
command! Jch JavaCallHierarchy
command! Jc  JavaCorrect
command! Jr  JavaRename
command! Jgs JavaGetSet
command! Jg  JavaGet
command! Js  JavaSet

command! DiffOrig vert new | set bt=nofile | r ++edit # | 0d_
      \ | diffthis | wincmd p | diffthis

" commands }}}1
" autocmd {{{1

if has("autocmd")
        filetype plugin on

        aug filetypes
                au!
                au BufNewFile,BufReadPost *.md set filetype=markdown
                au FileType c    setl ts=8 sw=8 et
                au FileType cpp  setl ts=8 sw=8 et
                au FileType sh   setl ts=8 sw=8 et
                au FileType make setl ts=8 sw=8 noet
                au FileType text setl ts=8 sw=8 et tw=72
                au FileType qf   wincmd J
                au QuickFixCmdPost * copen
        aug END

        aug vimrc
                au!
                au BufWritePost vimrc  source %
        aug END

        " got this aug from derekwyatt's vimrc
        aug Binary
                au!
                au BufReadPre   *.bin let &bin=1
                au BufReadPost  *.bin if &bin | %!xxd
                au BufReadPost  *.bin set filetype=xxd | endif
                au BufWritePre  *.bin if &bin | %!xxd -r
                au BufWritePre  *.bin endif
                au BufWritePost *.bin if &bin | %!xxd
                au BufWritePost *.bin set nomod | endif
        aug END
endif

" autocmd }}}1
" ~/.vimrc.local {{{1

if filereadable(expand("~/.vimrc.local"))
        source ~/.vimrc.local
endif

" ~/.vimrc.local }}}1
