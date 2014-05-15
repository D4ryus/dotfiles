" file: vimrc
" author: d4ryus - https://github.com/d4ryus/
" vim:ts=2:sw=2:foldmethod=marker:

" vundle {{{1

" settings {{{2

filetype off
set nocompatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Bundle's {{{2

Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'msanders/snipmate.vim'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'Lokaltog/vim-powerline'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'dhruvasagar/vim-table-mode'
Bundle 'tomasr/molokai'
Bundle 'vim-scripts/LustyJuggler'
Bundle 'vim-scripts/taglist.vim'
filetype plugin on

" abbreviations {{{1

iabbr author author: d4ryus - https://github.com/d4ryus/
iabbr file: file: <c-r>%
iabbr #i #include
iabbr #d #define

" macros {{{1

" recursive fold macro {{{2

map :fts zt,,f{azfa{j

" insert license {{{2

map :haw :0r ~/.vim/license/haw.txt<CR>

" setter {{{1

syntax on                       " enable syntax highlighting
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
set foldcolumn=3                " foldcolumn on the left side
set modelines=40                " search first/last 40 lines for vim modeline
set laststatus=2                " allways show statusline, since its powerline
set statusline=%{fugitive#statusline()} " fugitive statusline
set spelllang=en,de             " set spelling language to english and german
let g:EclimLoggingDisabled=1    " disable Eclim logging

" commands {{{1

:command! WQ wq
:command! Wq wq
:command! W w
:command! Q q

" autocmd {{{1

autocmd BufRead,BufNewFile *.txt setlocal spell " enable spellchecking on .txt files

" registers {{{1

let @f='"lyyO/* --fixme-- */"lpd2f|i/*A */==:w'
let @l='2fl"udwxxll"upa, jk'

" mapings {{{1

let mapleader=','

" map {{{2

map <Leader>  <Plug>(easymotion-prefix)
map <Tab>     %

map <Leader>n <Esc>:NERDTreeToggle<CR>
map <Leader>p <Esc>:ProjectProblems<CR>
map <Leader>m :Ant magic<CR>
map <Leader>t :TlistToggle<CR>

" nmap {{{2

nmap <Leader>b :LustyJuggler<CR>
nmap <Up>      :res +1<CR>
nmap <Down>    :res -1<CR>
nmap <Left>    :vertical res -1<CR>
nmap <Right>   :vertical res +1<CR>

" nnoremap {{{2

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" imap {{{2

imap jk <Esc>

" inoremap {{{2

inoremap <C-U> <C-G>u<C-U>

" vnoremap {{{2

vnoremap < <gv
vnoremap > >gv

" random {{{1

" make 81st column stand out {{{2
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%80v', 100)

" colorscheme {{{2

set background=light
colorscheme molokai
