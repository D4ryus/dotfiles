" file: ~/.vimrc
" author: d4ryus - https://github.com/d4ryus/
" vim:ts=8:sw=8:foldmethod=marker:

" vundle {{{1

" settings {{{2

filetype off
set nocompatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" }}}2

Bundle 'gmarik/vundle'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-git'
Bundle 'sjl/gundo.vim'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'kien/ctrlp.vim'
Bundle 'dhruvasagar/vim-table-mode'
Bundle 'gregsexton/gitv'
Bundle 'godlygeek/tabular'
Bundle 'terryma/vim-multiple-cursors'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/syntastic'
Bundle 'scrooloose/NERDTree'
Bundle 'nelstrom/vim-markdown-folding'

if executable('ctags')
        Bundle 'vim-scripts/taglist.vim'
endif

if has("python") || has("python3")
        Bundle 'SirVer/ultisnips'
endif

" Plugin-settings {{{1

" NERDtree {{{2

let NERDTreeDirArrows=0

" UltiSnips {{{2

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

" Eclim {{{2

let g:EclimLoggingDisabled=1    " disable Eclim logging
let g:EclimCompletionMethod='omnifunc'

" Supertab {{{2

let g:SuperTabDefaultCompletionType='context'

" Ag {{{2

if executable('ag')
        set grepprg=ag\ --nogroup\ --nocolor
        let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
        let g:ctrlp_use_caching = 0
endif

"}}}2

" macros {{{1

map :fts zt,,f{azfa{j
map :haw :0r ~/.vim/license/haw.txt<CR>

" setter {{{1

syntax on                       " enable syntax highlighting
set ruler                       " show the cursor position all the time
set showcmd                     " display incomplete commands
set incsearch                   " do incremental searching
set ignorecase                  " dont use case sensetive search
set nocompatible                " set noncompatible mode (vi vim)
set number                      " set linenumber on left side
set relativenumber              " set a relative number scale on left side
set autoread                    " autoread file when changed from outside
set autoindent                  " always set autoindenting on
set list                        " list all tabs and ending spaces
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set wrap                        " do not insert line break
set expandtab                   " use spaces instead of tabs
set tabstop=8                   " amout of spaces per tab
set shiftwidth=8                " number of spaces used by autoindent
set cm=blowfish                 " use blowfish as encryption (X)
set history=82                  " keep 82 lines of command line history
set listchars=tab:>-,nbsp:_,trail:.
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set encoding=UTF-8              " use UTF-8 as encoding
set modelines=40                " search first/last 40 lines for vim modeline options
set laststatus=2                " allways show statusline
set spelllang=en,de             " set spelling language to english and german
set directory=~/.vim/swap       " directory where all swap files will be
set foldtext=NeatFoldText()     " set foldtext to function below
set completeopt=longest,menuone " dont select the first match
set splitright                  " open splits on the right side instead of left
set splitbelow                  " open splits blow instead of on top

" registers {{{1

let @f='"lyyO/* --fixme-- */"lpd2f|i/*A */==:w'

" mapings {{{1

" noremap {{{2

noremap <Leader>t :TlistToggle<CR>
noremap <Leader>u :GundoToggle<CR>
noremap zv zMzv
noremap tab :Tabularize /

nnoremap <Up>    :res +1<CR>
nnoremap <Down>  :res -1<CR>
nnoremap <Left>  :vertical res -1<CR>
nnoremap <Right> :vertical res +1<CR>

" inoremap {{{2

inoremap jk <Esc>

" vnoremap {{{2

vnoremap < <gv
vnoremap > >gv

"}}}2

" abbrev {{{1

iabbrev date- <c-r>=strftime("%Y-%m-%d")<cr>
iabbrev file- <c-r>%<cr>

" appearance {{{1

" statusline {{{2

set statusline=%<[%F]\ [%{&ff}]\ [%M%Y%R%q%W]\ %{fugitive#statusline()}%=\ [pos:\ %l/%L:%c\ %p%%]

" }}}2
" }}}2
" overlength {{{2

highlight OverLength ctermbg=233 guibg=#592929
match OverLength /\%81v.\+/

" }}}2
" colorscheme {{{2

if &term =~ "xterm"                 ||
\  &term =~ "urxvt"                 ||
\  &term =~ "xterm-256color"        ||
\  &term =~ "screen-256color"       ||
\  &term =~ "rxvt-unicode-256color" ||
\  has("gui_running")
        colorscheme d4ryus_256
else
        colorscheme d4ryus_8
endif


" functions {{{1

function! NeatFoldText() "{{{2
" got this function from http://dhruvasagar.com/tag/vim thanks alot :)
        let line             = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
        let lines_count      = v:foldend - v:foldstart + 1
        let lines_count_text = '| ' . printf("%9s", lines_count . ' lines') . ' |'
        let foldchar         = matchstr(&fillchars, 'fold:\zs.')
        let foldtextstart    = strpart('+' . repeat(foldchar, v:foldlevel) . line, 0, (winwidth(0)*2)/3)
        let foldtextend      = lines_count_text . repeat(foldchar, 8)
        let foldtextlength   = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
        return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction " }}}2
function! NumberFold() "{{{2
        let h = matchstr(getline(v:lnum), '^\d\+')
        if empty(h)
                return "="
        else
                return ">" . len(h)
        endif
endfunction "}}}2
function! ApplyCodeStyle() "{{{2
        " fix else
        silent! %s/}[\r\n]else[\r\n]{/} else {/g
        " fix function
        silent! %s/.\+(.*).*\zs{\(.*$\)/\1\r{/g
        " remove all trailing whitespace's
        silent!  %s/\s\+$//e
endfunction "}}}2
function! RemoveTrailingWhitespaces() "{{{2
        silent! %s/\s\+$//e
endfunction "}}}2

" commands {{{1

command! WQ wq
command! Wq wq
command! W w
command! Q q

command! Codestyle call ApplyCodeStyle()
command! Rtw call RemoveTrailingWhitespaces()

command! Jio JavaImportOrganize
command! Jdp JavaDocPreview
command! Jch JavaCallHierarchy
command! Jc  JavaCorrect
command! Jr  JavaRename
command! Jgs JavaGetSet
command! Jg  JavaGet
command! Js  JavaSet

command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_
      \ | diffthis | wincmd p | diffthis
" autocmd {{{1

if has("autocmd")
        filetype plugin on
        autocmd BufNewFile,BufReadPost *.md set filetype=markdown
        autocmd FileType c          setlocal ts=8 sw=8 expandtab
        autocmd FileType cpp        setlocal ts=8 sw=8 expandtab
        autocmd FileType sh         setlocal ts=8 sw=8 expandtab
        autocmd FileType make       setlocal ts=8 sw=8 noexpandtab
endif
