" file: vimrc
" author: d4ryus - https://github.com/d4ryus/
" vim:ts=2:sw=2:foldmethod=marker:

" vundle {{{1

" settings {{{2

filetype off
set nocompatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" }}}2

Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'godlygeek/tabular'
Bundle 'tomasr/molokai'
Bundle 'vim-scripts/taglist.vim'
Bundle 'sjl/gundo.vim'
Bundle 'edkolev/tmuxline.vim'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'SirVer/ultisnips'
Bundle 'kien/ctrlp.vim'
Bundle 'rking/ag.vim'

" macros {{{1

map :fts zt,,f{azfa{j
map :haw :0r ~/.vim/license/haw.txt<CR>

" setter {{{1

syntax on                       " enable syntax highlighting
filetype plugin on              " enable filetype plugin
set cm=blowfish                 " use blowfish as encryption (X)
set autoindent                  " always set autoindenting on
set history=82                  " keep 82 lines of command line history
set ruler                       " show the cursor position all the time
set showcmd                     " display incomplete commands
set incsearch                   " do incremental searching
set hlsearch                    " highlight search
set ignorecase                  " dont use case sensetive search
set nocompatible                " set noncompatible mode (vi vim)
set number                      " set linenumber on left side
set relativenumber              " set a relative number scale on left side
set expandtab                   " use spaces instead of tabs
set tabstop=4                   " amout of spaces per tab
set shiftwidth=4                " number of spaces used by autoindent
set autoread                    " autoread file when changed from outside
set listchars=tab:>-,nbsp:_,trail:.
set list                        " list all tabs and ending spaces
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set encoding=UTF-8              " use UTF-8 as encoding
set t_Co=256                    " set Terminal color to 256
set nowrap                      " do not insert line break
set modelines=40                " search first/last 40 lines for vim modeline options
set laststatus=2                " allways show statusline
set spelllang=en,de             " set spelling language to english and german
set directory=~/.vim/swap       " set swap directory
let g:EclimLoggingDisabled=1    " disable Eclim logging
let g:tmuxline_powerline_separators=0
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

" commands {{{1

:command! WQ wq
:command! Wq wq
:command! W w
:command! Q q

" registers {{{1

let @f='"lyyO/* --fixme-- */"lpd2f|i/*A */==:w'
let @l='2fl"udwxxll"upa, jk'

" mapings {{{1

let mapleader=','

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" noremap {{{2

noremap <Leader>n <Esc>:NERDTreeToggle<CR>
noremap <Leader>p <Esc>:ProjectProblems<CR>
noremap <Leader>m :Ant magic<CR>
noremap <Leader>t :TlistToggle<CR>
noremap <Leader>u :GundoToggle<CR>

noremap <Leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>
noremap <Leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
noremap <Leader>ev :vsp <C-R>=expand("%:p:h") . "/" <CR>

nnoremap <Up>      :res +1<CR>
nnoremap <Down>    :res -1<CR>
nnoremap <Left>    :vertical res -1<CR>
nnoremap <Right>   :vertical res +1<CR>

" inoremap {{{2
"
inoremap jk <Esc>

inoremap <C-U> <C-G>u<C-U>
inoremap <silent><Bar> <Bar><Esc>:call <SID>align()<CR>a

" vnoremap {{{2

vnoremap < <gv
vnoremap > >gv

" color stuff {{{1

highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%80v', 100)

set background=light
colorscheme molokai

let g:airline_theme='simple'
let g:airline_left_sep=''
let g:airline_right_sep=''
"let g:airline_left_sep = '▶'
"let g:airline_right_sep = '◀'

" functions {{{1

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction

" autocmd {{{1

" remove all trailing whitespace's
augroup whitespaces
  autocmd BufWritePre * :%s/\s\+$//e
augroup END
